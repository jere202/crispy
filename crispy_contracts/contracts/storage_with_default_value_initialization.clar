;; Advanced Storage Contract with Extended Features
;; This contract demonstrates comprehensive storage operations with multiple data types and advanced functionality

;; ===============================
;; DATA VARIABLES & STORAGE
;; ===============================

;; Primary stored value with default
(define-data-var stored-value uint u42)

;; Additional storage variables
(define-data-var contract-name (string-ascii 50) "StorageContract")
(define-data-var is-active bool true)
(define-data-var last-updated uint block-height)
(define-data-var update-count uint u0)
(define-data-var max-value uint u1000000)
(define-data-var min-value uint u0)

;; Maps for advanced functionality
(define-map user-values principal uint)
(define-map user-permissions principal bool)
(define-map value-history uint {value: uint, updater: principal, block: uint})
(define-map user-stats principal {total-updates: uint, last-value: uint, joined-block: uint})

;; Lists for tracking
(define-data-var authorized-users (list 10 principal) (list))
(define-data-var value-snapshots (list 20 uint) (list))

;; ===============================
;; CONSTANTS & ERRORS
;; ===============================

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-VALUE (err u101))
(define-constant ERR-CONTRACT-PAUSED (err u102))
(define-constant ERR-VALUE-TOO-HIGH (err u103))
(define-constant ERR-VALUE-TOO-LOW (err u104))
(define-constant ERR-USER-NOT-FOUND (err u105))
(define-constant ERR-ALREADY-AUTHORIZED (err u106))
(define-constant ERR-HISTORY-FULL (err u107))
(define-constant ERR-INSUFFICIENT-BALANCE (err u108))

(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-HISTORY-SIZE u100)

;; ===============================
;; INITIALIZATION
;; ===============================

(define-private (initialize)
  (begin
    (var-set stored-value u42)
    (var-set last-updated block-height)
    (map-set user-permissions CONTRACT-OWNER true)
    (var-set authorized-users (list CONTRACT-OWNER))))

;; Call initialize on deployment
(initialize)

;; ===============================
;; READ-ONLY FUNCTIONS (GETTERS)
;; ===============================

;; Basic getters
(define-read-only (get-stored-value)
  (var-get stored-value))

(define-read-only (get-contract-name)
  (var-get contract-name))

(define-read-only (is-contract-active)
  (var-get is-active))

(define-read-only (get-last-updated)
  (var-get last-updated))

(define-read-only (get-update-count)
  (var-get update-count))

(define-read-only (get-value-limits)
  {min: (var-get min-value), max: (var-get max-value)})

;; User-specific getters
(define-read-only (get-user-value (user principal))
  (map-get? user-values user))

(define-read-only (is-user-authorized (user principal))
  (default-to false (map-get? user-permissions user)))

(define-read-only (get-user-stats (user principal))
  (map-get? user-stats user))

(define-read-only (get-authorized-users)
  (var-get authorized-users))

;; History and analytics
(define-read-only (get-value-history (index uint))
  (map-get? value-history index))

(define-read-only (get-value-snapshots)
  (var-get value-snapshots))

(define-read-only (get-contract-info)
  {
    name: (var-get contract-name),
    active: (var-get is-active),
    stored-value: (var-get stored-value),
    last-updated: (var-get last-updated),
    update-count: (var-get update-count),
    owner: CONTRACT-OWNER,
    current-block: block-height
  })

;; ===============================
;; AUTHORIZATION FUNCTIONS
;; ===============================

(define-public (add-authorized-user (user principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-user-authorized user)) ERR-ALREADY-AUTHORIZED)
    
    (map-set user-permissions user true)
    (map-set user-stats user {
      total-updates: u0,
      last-value: u0,
      joined-block: block-height
    })
    
    ;; Add to authorized users list
    (var-set authorized-users 
      (unwrap-panic (as-max-len? 
        (append (var-get authorized-users) user) u10)))
    
    (ok true)))

(define-public (remove-authorized-user (user principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-eq user CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
    
    (map-delete user-permissions user)
    (ok true)))

;; ===============================
;; VALUE MANAGEMENT FUNCTIONS
;; ===============================

(define-public (set-stored-value (new-value uint))
  (begin
    (asserts! (var-get is-active) ERR-CONTRACT-PAUSED)
    (asserts! (is-user-authorized tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (>= new-value (var-get min-value)) ERR-VALUE-TOO-LOW)
    (asserts! (<= new-value (var-get max-value)) ERR-VALUE-TOO-HIGH)
    
    ;; Update value and metadata
    (var-set stored-value new-value)
    (var-set last-updated block-height)
    (var-set update-count (+ (var-get update-count) u1))
    
    ;; Record in history
    (record-value-change new-value)
    
    ;; Update user stats
    (update-user-stats tx-sender new-value)
    
    ;; Add to snapshots
    (add-value-snapshot new-value)
    
    (ok new-value)))

(define-public (set-user-value (new-value uint))
  (begin
    (asserts! (var-get is-active) ERR-CONTRACT-PAUSED)
    (asserts! (>= new-value (var-get min-value)) ERR-VALUE-TOO-LOW)
    (asserts! (<= new-value (var-get max-value)) ERR-VALUE-TOO-HIGH)
    
    (map-set user-values tx-sender new-value)
    (update-user-stats tx-sender new-value)
    
    (ok new-value)))

(define-public (increment-value (amount uint))
  (let ((current-value (var-get stored-value))
        (new-value (+ current-value amount)))
    (begin
      (asserts! (var-get is-active) ERR-CONTRACT-PAUSED)
      (asserts! (is-user-authorized tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (<= new-value (var-get max-value)) ERR-VALUE-TOO-HIGH)
      
      (var-set stored-value new-value)
      (var-set last-updated block-height)
      (var-set update-count (+ (var-get update-count) u1))
      
      (record-value-change new-value)
      (update-user-stats tx-sender new-value)
      (add-value-snapshot new-value)
      
      (ok new-value))))

(define-public (decrement-value (amount uint))
  (let ((current-value (var-get stored-value)))
    (begin
      (asserts! (var-get is-active) ERR-CONTRACT-PAUSED)
      (asserts! (is-user-authorized tx-sender) ERR-NOT-AUTHORIZED)
      (asserts! (>= current-value amount) ERR-VALUE-TOO-LOW)
      
      (let ((new-value (- current-value amount)))
        (begin
          (asserts! (>= new-value (var-get min-value)) ERR-VALUE-TOO-LOW)
          
          (var-set stored-value new-value)
          (var-set last-updated block-height)
          (var-set update-count (+ (var-get update-count) u1))
          
          (record-value-change new-value)
          (update-user-stats tx-sender new-value)
          (add-value-snapshot new-value)
          
          (ok new-value))))))

;; ===============================
;; CONFIGURATION FUNCTIONS
;; ===============================

(define-public (set-contract-name (new-name (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set contract-name new-name)
    (ok new-name)))

(define-public (set-value-limits (new-min uint) (new-max uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (< new-min new-max) ERR-INVALID-VALUE)
    
    (var-set min-value new-min)
    (var-set max-value new-max)
    
    (ok {min: new-min, max: new-max})))

(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set is-active false)
    (ok true)))

(define-public (resume-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set is-active true)
    (ok true)))

(define-public (reset-to-default)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set stored-value u42)
    (var-set last-updated block-height)
    (var-set update-count (+ (var-get update-count) u1))
    
    (record-value-change u42)
    (add-value-snapshot u42)
    
    (ok u42)))

;; ===============================
;; UTILITY & HELPER FUNCTIONS
;; ===============================

(define-private (record-value-change (new-value uint))
  (let ((current-count (var-get update-count)))
    (map-set value-history current-count {
      value: new-value,
      updater: tx-sender,
      block: block-height
    })))

(define-private (update-user-stats (user principal) (value uint))
  (let ((current-stats (default-to 
                          {total-updates: u0, last-value: u0, joined-block: block-height}
                          (map-get? user-stats user))))
    (map-set user-stats user {
      total-updates: (+ (get total-updates current-stats) u1),
      last-value: value,
      joined-block: (get joined-block current-stats)
    })))

(define-private (add-value-snapshot (value uint))
  (let ((current-snapshots (var-get value-snapshots)))
    (if (< (len current-snapshots) u20)
        (var-set value-snapshots 
          (unwrap-panic (as-max-len? (append current-snapshots value) u20)))
        ;; If full, remove first and add new
        (var-set value-snapshots 
          (unwrap-panic (as-max-len? 
            (append (unwrap-panic (slice? current-snapshots u1 u20)) value) u20))))))

;; ===============================
;; HELPER FUNCTIONS FOR ANALYTICS
;; ===============================

;; Custom min function for fold operations
(define-private (get-min (a uint) (b uint))
  (if (< a b) a b))

;; Custom max function for fold operations
(define-private (get-max (a uint) (b uint))
  (if (> a b) a b))

;; ===============================
;; ADVANCED ANALYTICS FUNCTIONS
;; ===============================

(define-read-only (calculate-average-value)
  (let ((snapshots (var-get value-snapshots))
        (count (len snapshots)))
    (if (> count u0)
        (ok (/ (fold + snapshots u0) count))
        (err u0))))

(define-read-only (get-value-range)
  (let ((snapshots (var-get value-snapshots)))
    (if (> (len snapshots) u0)
        (ok {
          min: (fold get-min snapshots (unwrap-panic (element-at snapshots u0))),
          max: (fold get-max snapshots (unwrap-panic (element-at snapshots u0)))
        })
        (err u0))))

;; ===============================
;; EMERGENCY FUNCTIONS
;; ===============================

(define-public (emergency-reset)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Reset all values to defaults
    (var-set stored-value u42)
    (var-set is-active true)
    (var-set last-updated block-height)
    (var-set update-count u0)
    (var-set value-snapshots (list))
    
    (ok true)))

;; Helper function to check contract owner
(define-read-only (is-contract-owner (user principal))
  (is-eq user CONTRACT-OWNER))

(define-read-only (get-contract-owner)
  CONTRACT-OWNER)