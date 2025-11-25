;; Enhanced Timed Storage Update Contract
;; Advanced features: multi-value storage, access control, history tracking, automation, and more

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-UPDATE-TOO-EARLY (err u101))
(define-constant ERR-INVALID-TIME (err u102))
(define-constant ERR-VALUE-NOT-FOUND (err u103))
(define-constant ERR-INVALID-KEY (err u104))
(define-constant ERR-SCHEDULE-FULL (err u105))
(define-constant ERR-INVALID-ROLE (err u106))
(define-constant ERR-CONTRACT-PAUSED (err u107))
(define-constant ERR-VALUE-OUT-OF_RANGE (err u108))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u109))
(define-constant ERR-EMERGENCY-MODE (err u110))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-SCHEDULED-UPDATES u50)
(define-constant MAX-HISTORY-ENTRIES u100)
(define-constant ADMIN-ROLE u1)
(define-constant UPDATER-ROLE u2)
(define-constant VIEWER-ROLE u3)

;; Data variables
(define-data-var stored-value uint u0)
(define-data-var next-update-block uint u0)
(define-data-var next-update-timestamp uint u0)
(define-data-var use-timestamp bool false)
(define-data-var contract-paused bool false)
(define-data-var emergency-mode bool false)
(define-data-var update-fee uint u0)
(define-data-var min-value uint u0)
(define-data-var max-value uint u1000000)
(define-data-var total-updates uint u0)
(define-data-var auto-update-enabled bool false)
(define-data-var update-interval uint u10)
(define-data-var scheduled-update-counter uint u0)

;; Maps for enhanced features
(define-map user-roles principal uint)
(define-map named-values (string-ascii 32) {value: uint, last-updated: uint, next-update: uint})
(define-map update-history uint {value: uint, updater: principal, timestamp: uint, block: uint})
(define-map scheduled-updates uint {key: (string-ascii 32), value: uint, target-block: uint, target-timestamp: uint, creator: principal})
(define-map access-permissions principal {can-read: bool, can-update: bool, expiry: uint})
(define-map value-validators (string-ascii 32) {min-val: uint, max-val: uint, required-role: uint})

;; Private functions

;; Check if caller has specific role
(define-private (has-role (user principal) (required-role uint))
  (let ((user-role (default-to u0 (map-get? user-roles user))))
    (or (is-eq user CONTRACT-OWNER) (>= user-role required-role))
  )
)

;; Check if contract is operational
(define-private (is-operational)
  (and (not (var-get contract-paused)) (not (var-get emergency-mode)))
)

;; Check if caller is authorized for updates
(define-private (can-update-value (user principal))
  (and 
    (is-operational)
    (has-role user UPDATER-ROLE)
    (match (map-get? access-permissions user)
      perm (and (get can-update perm) (> (get expiry perm) block-height))
      (has-role user UPDATER-ROLE))
  )
)

;; Check timing constraints
(define-private (can-update-by-block)
  (>= block-height (var-get next-update-block))
)

(define-private (can-update-by-timestamp)
  (match (get-block-info? time (- block-height u1))
    current-time (>= current-time (var-get next-update-timestamp))
    false
  )
)

(define-private (can-update-now)
  (if (var-get use-timestamp)
    (can-update-by-timestamp)
    (can-update-by-block)
  )
)

;; Validate value against constraints
(define-private (is-valid-value (value uint))
  (and (>= value (var-get min-value)) (<= value (var-get max-value)))
)

;; Add to history
(define-private (add-to-history (value uint))
  (let ((update-count (var-get total-updates)))
    (map-set update-history update-count 
      {
        value: value,
        updater: tx-sender,
        timestamp: (default-to u0 (get-block-info? time (- block-height u1))),
        block: block-height
      })
    (var-set total-updates (+ update-count u1))
  )
)

;; Process scheduled updates (helper for fold)
(define-private (process-scheduled-update (schedule-id uint) (prev (response bool uint)))
  (match prev
    success
    (match (map-get? scheduled-updates schedule-id)
      schedule
      (let ((can-execute (if (var-get use-timestamp)
                           (>= (default-to u0 (get-block-info? time (- block-height u1))) (get target-timestamp schedule))
                           (>= block-height (get target-block schedule)))))
        (if can-execute
          (begin
            (map-set named-values (get key schedule)
              {
                value: (get value schedule),
                last-updated: block-height,
                next-update: (if (var-get use-timestamp)
                              (+ (default-to u0 (get-block-info? time (- block-height u1))) (var-get update-interval))
                              (+ block-height (var-get update-interval)))
              })
            (map-delete scheduled-updates schedule-id)
            (ok true))
          (ok false)))
      (ok false))
    error (err error)
  )
)

;; Public functions

;; Initialize contract with comprehensive settings
(define-public (initialize (initial-value uint) 
                          (delay-blocks uint) 
                          (delay-timestamp uint) 
                          (use-time bool)
                          (fee uint)
                          (min-val uint)
                          (max-val uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set stored-value initial-value)
    (var-set next-update-block (+ block-height delay-blocks))
    (var-set next-update-timestamp (+ (default-to u0 (get-block-info? time (- block-height u1))) delay-timestamp))
    (var-set use-timestamp use-time)
    (var-set update-fee fee)
    (var-set min-value min-val)
    (var-set max-value max-val)
    (add-to-history initial-value)
    (ok true)
  )
)

;; Enhanced update with payment and validation
(define-public (update-value (new-value uint) (next-delay uint))
  (let ((fee (var-get update-fee)))
    (begin
      (asserts! (can-update-value tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (can-update-now) ERR-UPDATE-TOO-EARLY)
      (asserts! (is-valid-value new-value) ERR-VALUE-OUT-OF_RANGE)
      
      ;; Handle payment if fee is set
      (if (> fee u0)
        (unwrap! (stx-transfer? fee tx-sender CONTRACT-OWNER) ERR-INSUFFICIENT-PAYMENT)
        true)
      
      ;; Update the stored value
      (var-set stored-value new-value)
      
      ;; Set next update time
      (if (var-get use-timestamp)
        (var-set next-update-timestamp 
          (+ (default-to u0 (get-block-info? time (- block-height u1))) next-delay))
        (var-set next-update-block (+ block-height next-delay)))
      
      ;; Add to history
      (add-to-history new-value)
      
      (ok true)
    )
  )
)

;; Multi-value storage system
(define-public (set-named-value (key (string-ascii 32)) (value uint) (delay uint))
  (begin
    (asserts! (can-update-value tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-valid-value value) ERR-VALUE-OUT-OF_RANGE)
    
    (map-set named-values key
      {
        value: value,
        last-updated: block-height,
        next-update: (if (var-get use-timestamp)
                      (+ (default-to u0 (get-block-info? time (- block-height u1))) delay)
                      (+ block-height delay))
      })
    (ok true)
  )
)

;; Schedule future updates
(define-public (schedule-update (key (string-ascii 32)) 
                               (value uint) 
                               (target-block uint) 
                               (target-timestamp uint))
  (let ((current-counter (var-get scheduled-update-counter)))
    (begin
      (asserts! (can-update-value tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (< current-counter MAX-SCHEDULED-UPDATES) ERR-SCHEDULE-FULL)
      (asserts! (is-valid-value value) ERR-VALUE-OUT-OF_RANGE)
      
      (map-set scheduled-updates current-counter
        {
          key: key,
          value: value,
          target-block: target-block,
          target-timestamp: target-timestamp,
          creator: tx-sender
        })
      (var-set scheduled-update-counter (+ current-counter u1))
      (ok current-counter)
    )
  )
)

;; Role management
(define-public (assign-role (user principal) (role uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= role VIEWER-ROLE) ERR-INVALID-ROLE)
    (map-set user-roles user role)
    (ok true)
  )
)

;; Access permission management
(define-public (grant-access (user principal) (can-read bool) (can-update bool) (duration uint))
  (begin
    (asserts! (has-role tx-sender ADMIN-ROLE) ERR-NOT-AUTHORIZED)
    (map-set access-permissions user
      {
        can-read: can-read,
        can-update: can-update,
        expiry: (+ block-height duration)
      })
    (ok true)
  )
)

;; Emergency controls
(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set contract-paused false)
    (ok true)
  )
)

(define-public (activate-emergency-mode)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set emergency-mode true)
    (ok true)
  )
)

;; Batch operations
(define-public (batch-update (updates (list 10 {key: (string-ascii 32), value: uint})))
  (begin
    (asserts! (can-update-value tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (can-update-now) ERR-UPDATE-TOO-EARLY)
    
    (fold process-batch-update updates (ok true))
  )
)

(define-private (process-batch-update (update {key: (string-ascii 32), value: uint}) (prev (response bool uint)))
  (match prev
    success (set-named-value (get key update) (get value update) (var-get update-interval))
    error (err error)
  )
)

;; Automated update processing
(define-public (process-scheduled-updates (schedule-ids (list 10 uint)))
  (begin
    (asserts! (var-get auto-update-enabled) ERR-NOT-AUTHORIZED)
    (fold process-scheduled-update schedule-ids (ok true))
  )
)

;; Configuration updates
(define-public (update-config (new-fee uint) (new-min uint) (new-max uint) (new-interval uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set update-fee new-fee)
    (var-set min-value new-min)
    (var-set max-value new-max)
    (var-set update-interval new-interval)
    (ok true)
  )
)

;; Read-only functions

;; Enhanced getters
(define-read-only (get-value)
  (var-get stored-value))

(define-read-only (get-named-value (key (string-ascii 32)))
  (map-get? named-values key))

(define-read-only (get-user-role (user principal))
  (default-to u0 (map-get? user-roles user)))

(define-read-only (get-access-permissions (user principal))
  (map-get? access-permissions user))

(define-read-only (get-history-entry (index uint))
  (map-get? update-history index))

(define-read-only (get-scheduled-update (schedule-id uint))
  (map-get? scheduled-updates schedule-id))

;; Status and analytics
(define-read-only (get-contract-status)
  {
    current-value: (var-get stored-value),
    total-updates: (var-get total-updates),
    next-update-block: (var-get next-update-block),
    next-update-timestamp: (var-get next-update-timestamp),
    using-timestamp: (var-get use-timestamp),
    is-paused: (var-get contract-paused),
    emergency-mode: (var-get emergency-mode),
    current-block: block-height,
    can-update: (can-update-now),
    update-fee: (var-get update-fee),
    value-range: {min: (var-get min-value), max: (var-get max-value)},
    auto-update-enabled: (var-get auto-update-enabled)
  }
)

(define-read-only (get-update-stats)
  {
    total-updates: (var-get total-updates),
    blocks-until-update: (if (can-update-by-block) u0 (- (var-get next-update-block) block-height)),
    update-interval: (var-get update-interval),
    last-update-block: (if (> (var-get total-updates) u0) 
                        (get block (default-to {value: u0, updater: tx-sender, timestamp: u0, block: u0} 
                                   (map-get? update-history (- (var-get total-updates) u1))))
                        u0)
  }
)

;; Security and validation
(define-read-only (can-user-update (user principal))
  (can-update-value user))

(define-read-only (is-value-valid (value uint))
  (is-valid-value value))

(define-read-only (get-required-fee)
  (var-get update-fee))

;; Historical data analysis
(define-read-only (get-value-trend (lookback uint))
  (let ((current-count (var-get total-updates))
        (start-index (if (> current-count lookback) (- current-count lookback) u0)))
    {
      current-value: (var-get stored-value),
      entries-analyzed: (if (> current-count lookback) lookback current-count),
      oldest-in-range: (map-get? update-history start-index)
    }
  )
)

;; Advanced queries
(define-read-only (get-comprehensive-info)
  {
    contract-status: (get-contract-status),
    update-stats: (get-update-stats),
    owner: CONTRACT-OWNER,
    contract-version: "2.0-enhanced"
  }
)