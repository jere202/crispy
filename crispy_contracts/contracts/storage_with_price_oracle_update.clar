;; Enhanced Price Oracle Storage Contract
;; This contract allows trusted oracles to update multiple price feeds with advanced features

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-VALUE (err u101))
(define-constant ERR-PRICE-TOO-OLD (err u102))
(define-constant ERR-PRICE-DEVIATION-TOO-HIGH (err u103))
(define-constant ERR-ORACLE-ALREADY-EXISTS (err u104))
(define-constant ERR-ORACLE-NOT-FOUND (err u105))
(define-constant ERR-PRICE-FEED-NOT-FOUND (err u106))
(define-constant ERR-CONTRACT-PAUSED (err u107))
(define-constant ERR-INVALID-DEVIATION-THRESHOLD (err u108))

;; Contract owner (deployer)
(define-constant CONTRACT-OWNER tx-sender)

;; Configuration constants
(define-constant MAX-PRICE-AGE u144) ;; ~24 hours in blocks (assuming 10min blocks)
(define-constant MAX-ORACLES u10)
(define-constant PRECISION u1000000) ;; 6 decimal places for percentage calculations

;; Data variables
(define-data-var contract-paused bool false)
(define-data-var oracle-count uint u0)
(define-data-var price-deviation-threshold uint u100000) ;; 10% in basis points (10 * PRECISION / 100)
(define-data-var emergency-admin (optional principal) none)

;; Maps for multiple price feeds
(define-map price-feeds 
  { symbol: (string-ascii 10) }
  { 
    price: uint,
    last-updated: uint,
    oracle: principal,
    previous-price: uint,
    update-count: uint
  }
)

;; Oracle management
(define-map authorized-oracles principal bool)
(define-map oracle-info 
  principal 
  { 
    authorized: bool,
    total-updates: uint,
    last-update: uint,
    reputation-score: uint
  }
)

;; Price history (last 5 prices for each symbol)
(define-map price-history 
  { symbol: (string-ascii 10), index: uint }
  { price: uint, timestamp: uint, oracle: principal }
)

;; Circuit breaker for individual price feeds
(define-map price-feed-status
  { symbol: (string-ascii 10) }
  { active: bool, last-check: uint }
)

;; Events map for tracking
(define-map daily-stats
  { date: uint }
  { updates: uint, unique-oracles: uint }
)

;; Private helper functions
(define-private (is-authorized-oracle)
  (default-to false (map-get? authorized-oracles tx-sender))
)

(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (is-emergency-admin)
  (match (var-get emergency-admin)
    admin (is-eq tx-sender admin)
    false
  )
)

(define-private (calculate-percentage-change (old-price uint) (new-price uint))
  (if (is-eq old-price u0)
    u0
    (let ((diff (if (> new-price old-price) 
                   (- new-price old-price) 
                   (- old-price new-price))))
      (/ (* diff PRECISION) old-price)
    )
  )
)

(define-private (update-price-history (symbol (string-ascii 10)) (price uint) (oracle principal))
  (let ((current-time block-height))
    ;; Shift history (simple implementation - store last 5 entries)
    (map-set price-history { symbol: symbol, index: u4 } 
             (default-to { price: u0, timestamp: u0, oracle: tx-sender } 
                        (map-get? price-history { symbol: symbol, index: u3 })))
    (map-set price-history { symbol: symbol, index: u3 } 
             (default-to { price: u0, timestamp: u0, oracle: tx-sender } 
                        (map-get? price-history { symbol: symbol, index: u2 })))
    (map-set price-history { symbol: symbol, index: u2 } 
             (default-to { price: u0, timestamp: u0, oracle: tx-sender } 
                        (map-get? price-history { symbol: symbol, index: u1 })))
    (map-set price-history { symbol: symbol, index: u1 } 
             (default-to { price: u0, timestamp: u0, oracle: tx-sender } 
                        (map-get? price-history { symbol: symbol, index: u0 })))
    (map-set price-history { symbol: symbol, index: u0 } 
             { price: price, timestamp: current-time, oracle: oracle })
  )
)

;; Admin functions
(define-public (pause-contract)
  (begin
    (asserts! (or (is-contract-owner) (is-emergency-admin)) ERR-NOT-AUTHORIZED)
    (var-set contract-paused true)
    (print { event: "contract-paused", admin: tx-sender })
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (var-set contract-paused false)
    (print { event: "contract-unpaused", admin: tx-sender })
    (ok true)
  )
)

(define-public (set-emergency-admin (admin (optional principal)))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (var-set emergency-admin admin)
    (print { event: "emergency-admin-updated", admin: admin })
    (ok true)
  )
)

(define-public (set-price-deviation-threshold (threshold uint))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (<= threshold (* u50 PRECISION u100)) ERR-INVALID-DEVIATION-THRESHOLD) ;; Max 50%
    (var-set price-deviation-threshold threshold)
    (print { event: "deviation-threshold-updated", threshold: threshold })
    (ok true)
  )
)

;; Oracle management functions
(define-public (add-oracle (oracle principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (< (var-get oracle-count) MAX-ORACLES) ERR-ORACLE-ALREADY-EXISTS)
    (asserts! (not (default-to false (map-get? authorized-oracles oracle))) ERR-ORACLE-ALREADY-EXISTS)
    
    (map-set authorized-oracles oracle true)
    (map-set oracle-info oracle { 
      authorized: true, 
      total-updates: u0, 
      last-update: u0,
      reputation-score: u100 ;; Start with perfect score
    })
    (var-set oracle-count (+ (var-get oracle-count) u1))
    
    (print { event: "oracle-added", oracle: oracle })
    (ok true)
  )
)

(define-public (remove-oracle (oracle principal))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (default-to false (map-get? authorized-oracles oracle)) ERR-ORACLE-NOT-FOUND)
    
    (map-set authorized-oracles oracle false)
    (map-set oracle-info oracle { 
      authorized: false, 
      total-updates: u0, 
      last-update: u0,
      reputation-score: u0
    })
    (var-set oracle-count (- (var-get oracle-count) u1))
    
    (print { event: "oracle-removed", oracle: oracle })
    (ok true)
  )
)

;; Main price update function with enhanced features
(define-public (update-price (symbol (string-ascii 10)) (new-price uint))
  (let (
    (current-feed (map-get? price-feeds { symbol: symbol }))
    (feed-status (default-to { active: true, last-check: u0 } 
                             (map-get? price-feed-status { symbol: symbol })))
  )
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (is-authorized-oracle) ERR-NOT-AUTHORIZED)
    (asserts! (> new-price u0) ERR-INVALID-VALUE)
    (asserts! (get active feed-status) ERR-PRICE-FEED-NOT-FOUND)
    
    (match current-feed
      feed (let (
        (price-change (calculate-percentage-change (get price feed) new-price))
        (oracle-data (default-to { authorized: false, total-updates: u0, last-update: u0, reputation-score: u100 }
                                 (map-get? oracle-info tx-sender)))
      )
        ;; Check price deviation
        (asserts! (<= price-change (var-get price-deviation-threshold)) ERR-PRICE-DEVIATION-TOO-HIGH)
        
        ;; Update price feed
        (map-set price-feeds { symbol: symbol } {
          price: new-price,
          last-updated: block-height,
          oracle: tx-sender,
          previous-price: (get price feed),
          update-count: (+ (get update-count feed) u1)
        })
        
        ;; Update oracle stats
        (map-set oracle-info tx-sender {
          authorized: true,
          total-updates: (+ (get total-updates oracle-data) u1),
          last-update: block-height,
          reputation-score: (get reputation-score oracle-data) ;; Could implement reputation logic
        })
        
        ;; Update price history
        (update-price-history symbol new-price tx-sender)
        
        (print {
          event: "price-updated",
          symbol: symbol,
          new-price: new-price,
          previous-price: (get price feed),
          oracle: tx-sender,
          block-height: block-height,
          price-change-percentage: price-change
        })
        (ok new-price)
      )
      ;; First time setting this price feed
      (begin
        (map-set price-feeds { symbol: symbol } {
          price: new-price,
          last-updated: block-height,
          oracle: tx-sender,
          previous-price: u0,
          update-count: u1
        })
        
        (map-set price-feed-status { symbol: symbol } { active: true, last-check: block-height })
        (update-price-history symbol new-price tx-sender)
        
        (print {
          event: "price-feed-created",
          symbol: symbol,
          initial-price: new-price,
          oracle: tx-sender,
          block-height: block-height
        })
        (ok new-price)
      )
    )
  )
)

;; Batch price update function
(define-public (batch-update-prices (updates (list 10 { symbol: (string-ascii 10), price: uint })))
  (begin
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    (asserts! (is-authorized-oracle) ERR-NOT-AUTHORIZED)
    
    (fold batch-update-helper updates (ok (list)))
  )
)

(define-private (batch-update-helper 
  (update { symbol: (string-ascii 10), price: uint }) 
  (prev-result (response (list 10 uint) uint)))
  (match prev-result
    success (match (update-price (get symbol update) (get price update))
      ok (ok (unwrap-panic (as-max-len? (append success ok) u10)))
      error (err error)
    )
    error (err error)
  )
)

;; Price feed management
(define-public (deactivate-price-feed (symbol (string-ascii 10)))
  (begin
    (asserts! (or (is-contract-owner) (is-emergency-admin)) ERR-NOT-AUTHORIZED)
    (map-set price-feed-status { symbol: symbol } { active: false, last-check: block-height })
    (print { event: "price-feed-deactivated", symbol: symbol })
    (ok true)
  )
)

(define-public (activate-price-feed (symbol (string-ascii 10)))
  (begin
    (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
    (map-set price-feed-status { symbol: symbol } { active: true, last-check: block-height })
    (print { event: "price-feed-activated", symbol: symbol })
    (ok true)
  )
)

;; Enhanced read-only functions
(define-read-only (get-price (symbol (string-ascii 10)))
  (match (map-get? price-feeds { symbol: symbol })
    feed (some (get price feed))
    none
  )
)

(define-read-only (get-price-info (symbol (string-ascii 10)))
  (map-get? price-feeds { symbol: symbol })
)

(define-read-only (get-price-with-freshness (symbol (string-ascii 10)))
  (match (map-get? price-feeds { symbol: symbol })
    feed (let ((age (- block-height (get last-updated feed))))
      (some {
        price: (get price feed),
        last-updated: (get last-updated feed),
        age: age,
        is-fresh: (<= age MAX-PRICE-AGE),
        oracle: (get oracle feed)
      })
    )
    none
  )
)

(define-read-only (get-price-history (symbol (string-ascii 10)) (index uint))
  (map-get? price-history { symbol: symbol, index: index })
)

(define-read-only (get-oracle-info (oracle principal))
  (map-get? oracle-info oracle)
)

(define-read-only (is-oracle-authorized (oracle principal))
  (default-to false (map-get? authorized-oracles oracle))
)

(define-read-only (get-contract-status)
  {
    paused: (var-get contract-paused),
    oracle-count: (var-get oracle-count),
    deviation-threshold: (var-get price-deviation-threshold),
    emergency-admin: (var-get emergency-admin),
    owner: CONTRACT-OWNER
  }
)

(define-read-only (get-price-feed-status (symbol (string-ascii 10)))
  (default-to { active: false, last-check: u0 } 
              (map-get? price-feed-status { symbol: symbol }))
)

(define-read-only (get-all-oracle-stats)
  {
    total-oracles: (var-get oracle-count),
    max-oracles: MAX-ORACLES,
    current-block: block-height
  }
)

;; Utility functions
(define-read-only (calculate-price-change-percentage (symbol (string-ascii 10)))
  (match (map-get? price-feeds { symbol: symbol })
    feed (if (> (get previous-price feed) u0)
           (some (calculate-percentage-change (get previous-price feed) (get price feed)))
           none)
    none
  )
)