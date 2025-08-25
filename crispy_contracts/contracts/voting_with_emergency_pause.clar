;; Enhanced Emergency Pause Voting Contract
;; Advanced voting system with comprehensive features

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-VOTING-PAUSED (err u101))
(define-constant ERR-ALREADY-VOTED (err u102))
(define-constant ERR-INVALID-OPTION (err u103))
(define-constant ERR-VOTING-NOT-PAUSED (err u104))
(define-constant ERR-VOTING-NOT-STARTED (err u105))
(define-constant ERR-VOTING-ENDED (err u106))
(define-constant ERR-INVALID-DURATION (err u107))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u108))
(define-constant ERR-PROPOSAL-ALREADY-EXISTS (err u109))
(define-constant ERR-INSUFFICIENT-TOKENS (err u110))
(define-constant ERR-BLACKLISTED (err u111))
(define-constant ERR-PROPOSAL-NOT-ACTIVE (err u112))
(define-constant ERR-MINIMUM-PARTICIPATION-NOT-MET (err u113))

;; Contract owner/admin
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MINIMUM-PARTICIPATION-THRESHOLD u10) ;; Minimum 10 votes for valid proposal

;; Data variables
(define-data-var voting-paused bool false)
(define-data-var pause-reason (string-ascii 256) "")
(define-data-var total-votes uint u0)
(define-data-var current-proposal-id uint u0)
(define-data-var voting-token-requirement uint u100) ;; Minimum tokens required to vote
(define-data-var admin-count uint u1)

;; Proposal structure
(define-map proposals uint {
  title: (string-ascii 256),
  description: (string-ascii 1024),
  creator: principal,
  start-block: uint,
  end-block: uint,
  options: (list 10 (string-ascii 128)),
  total-votes: uint,
  is-active: bool,
  min-participation: uint
})

;; Vote tracking
(define-map votes {proposal-id: uint, voter: principal} uint)
(define-map proposal-vote-counts {proposal-id: uint, option: uint} uint)
(define-map user-voting-power principal uint)
(define-map blacklisted-users principal bool)

;; Admin management
(define-map admins principal bool)

;; Voting history and analytics
(define-map user-vote-history principal (list 50 uint))
(define-map proposal-voters uint (list 1000 principal))

;; Helper functions
(define-private (max-uint (a uint) (b uint))
  (if (>= a b) a b)
)

;; Events/logs (using print for event logging)
(define-private (log-event (event (string-ascii 64)) (data (string-ascii 256)))
  (print {event: event, data: data, block-height: block-height})
)

;; Initialize contract
(map-set admins CONTRACT-OWNER true)

;; Read-only functions
(define-read-only (is-voting-paused)
  (var-get voting-paused)
)

(define-read-only (get-pause-reason)
  (var-get pause-reason)
)

(define-read-only (is-admin (user principal))
  (default-to false (map-get? admins user))
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-current-proposal-id)
  (var-get current-proposal-id)
)

(define-read-only (get-user-vote (proposal-id uint) (user principal))
  (map-get? votes {proposal-id: proposal-id, voter: user})
)

(define-read-only (get-proposal-vote-count (proposal-id uint) (option uint))
  (default-to u0 (map-get? proposal-vote-counts {proposal-id: proposal-id, option: option}))
)

(define-read-only (get-voting-power (user principal))
  (default-to u0 (map-get? user-voting-power user))
)

(define-read-only (is-blacklisted (user principal))
  (default-to false (map-get? blacklisted-users user))
)

(define-read-only (has-user-voted (proposal-id uint) (user principal))
  (is-some (map-get? votes {proposal-id: proposal-id, voter: user}))
)

(define-read-only (get-token-requirement)
  (var-get voting-token-requirement)
)

(define-read-only (is-proposal-active (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal 
    (and 
      (get is-active proposal)
      (>= block-height (get start-block proposal))
      (<= block-height (get end-block proposal))
      (not (is-voting-paused))
    )
    false
  )
)

(define-read-only (get-proposal-results (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal
    (some {
      proposal-id: proposal-id,
      title: (get title proposal),
      total-votes: (get total-votes proposal),
      option-1: (get-proposal-vote-count proposal-id u1),
      option-2: (get-proposal-vote-count proposal-id u2),
      option-3: (get-proposal-vote-count proposal-id u3),
      option-4: (get-proposal-vote-count proposal-id u4),
      option-5: (get-proposal-vote-count proposal-id u5),
      is-active: (is-proposal-active proposal-id),
      participation-met: (>= (get total-votes proposal) (get min-participation proposal))
    })
    none
  )
)

;; Admin functions
(define-public (add-admin (new-admin principal))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (map-set admins new-admin true)
    (var-set admin-count (+ (var-get admin-count) u1))
    (log-event "ADMIN_ADDED" "New admin added")
    (ok true)
  )
)

(define-public (remove-admin (admin principal))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (asserts! (not (is-eq admin CONTRACT-OWNER)) ERR-UNAUTHORIZED) ;; Cannot remove contract owner
    (map-delete admins admin)
    (var-set admin-count (- (var-get admin-count) u1))
    (log-event "ADMIN_REMOVED" "Admin removed")
    (ok true)
  )
)

(define-public (pause-voting (reason (string-ascii 256)))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (asserts! (not (var-get voting-paused)) ERR-VOTING-PAUSED)
    (var-set voting-paused true)
    (var-set pause-reason reason)
    (log-event "VOTING_PAUSED" reason)
    (ok true)
  )
)

(define-public (unpause-voting)
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (asserts! (var-get voting-paused) ERR-VOTING-NOT-PAUSED)
    (var-set voting-paused false)
    (var-set pause-reason "")
    (log-event "VOTING_UNPAUSED" "Voting resumed")
    (ok true)
  )
)

(define-public (set-token-requirement (new-requirement uint))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (var-set voting-token-requirement new-requirement)
    (log-event "TOKEN_REQUIREMENT_UPDATED" "Token requirement changed")
    (ok true)
  )
)

(define-public (blacklist-user (user principal))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (map-set blacklisted-users user true)
    (log-event "USER_BLACKLISTED" "User blacklisted")
    (ok true)
  )
)

(define-public (unblacklist-user (user principal))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (map-delete blacklisted-users user)
    (log-event "USER_UNBLACKLISTED" "User unblacklisted")
    (ok true)
  )
)

(define-public (set-voting-power (user principal) (power uint))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (map-set user-voting-power user power)
    (log-event "VOTING_POWER_SET" "User voting power updated")
    (ok true)
  )
)

;; Proposal management
(define-public (create-proposal 
  (title (string-ascii 256)) 
  (description (string-ascii 1024))
  (duration-blocks uint)
  (options (list 10 (string-ascii 128)))
  (min-participation uint)
)
  (let (
    (proposal-id (+ (var-get current-proposal-id) u1))
    (start-block (+ block-height u1))
    (end-block (+ block-height duration-blocks))
  )
    (asserts! (> duration-blocks u0) ERR-INVALID-DURATION)
    (asserts! (not (is-voting-paused)) ERR-VOTING-PAUSED)
    (asserts! (not (is-blacklisted tx-sender)) ERR-BLACKLISTED)
    
    (map-set proposals proposal-id {
      title: title,
      description: description,
      creator: tx-sender,
      start-block: start-block,
      end-block: end-block,
      options: options,
      total-votes: u0,
      is-active: true,
      min-participation: min-participation
    })
    
    (var-set current-proposal-id proposal-id)
    (log-event "PROPOSAL_CREATED" title)
    (ok proposal-id)
  )
)

(define-public (deactivate-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
  )
    (asserts! (or (is-admin tx-sender) (is-eq tx-sender (get creator proposal))) ERR-UNAUTHORIZED)
    
    (map-set proposals proposal-id (merge proposal {is-active: false}))
    (log-event "PROPOSAL_DEACTIVATED" "Proposal deactivated")
    (ok true)
  )
)

;; Enhanced voting functions
(define-public (cast-vote (proposal-id uint) (option uint))
  (let (
    (voter tx-sender)
    (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
    (current-count (get-proposal-vote-count proposal-id option))
    (voter-power (max-uint u1 (get-voting-power voter)))
  )
    ;; Validation checks
    (asserts! (is-proposal-active proposal-id) ERR-PROPOSAL-NOT-ACTIVE)
    (asserts! (not (is-blacklisted voter)) ERR-BLACKLISTED)
    (asserts! (not (has-user-voted proposal-id voter)) ERR-ALREADY-VOTED)
    (asserts! (>= (get-voting-power voter) (var-get voting-token-requirement)) ERR-INSUFFICIENT-TOKENS)
    (asserts! (and (>= option u1) (<= option u5)) ERR-INVALID-OPTION)
    
    ;; Record the vote
    (map-set votes {proposal-id: proposal-id, voter: voter} option)
    
    ;; Update vote counts with voting power
    (map-set proposal-vote-counts {proposal-id: proposal-id, option: option} (+ current-count voter-power))
    
    ;; Update proposal total votes
    (map-set proposals proposal-id (merge proposal {total-votes: (+ (get total-votes proposal) voter-power)}))
    
    ;; Update global total
    (var-set total-votes (+ (var-get total-votes) voter-power))
    
    ;; Add to voter list for this proposal
    (let (
      (current-voters (default-to (list) (map-get? proposal-voters proposal-id)))
    )
      (map-set proposal-voters proposal-id (unwrap! (as-max-len? (append current-voters voter) u1000) (ok option)))
    )
    
    (log-event "VOTE_CAST" "Vote recorded")
    (ok option)
  )
)

(define-public (change-vote (proposal-id uint) (new-option uint))
  (let (
    (voter tx-sender)
    (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
    (old-option (unwrap! (get-user-vote proposal-id voter) ERR-ALREADY-VOTED))
    (old-count (get-proposal-vote-count proposal-id old-option))
    (new-count (get-proposal-vote-count proposal-id new-option))
    (voter-power (max-uint u1 (get-voting-power voter)))
  )
    ;; Validation checks
    (asserts! (is-proposal-active proposal-id) ERR-PROPOSAL-NOT-ACTIVE)
    (asserts! (not (is-blacklisted voter)) ERR-BLACKLISTED)
    (asserts! (and (>= new-option u1) (<= new-option u5)) ERR-INVALID-OPTION)
    
    ;; Update vote record
    (map-set votes {proposal-id: proposal-id, voter: voter} new-option)
    
    ;; Update vote counts
    (map-set proposal-vote-counts {proposal-id: proposal-id, option: old-option} (- old-count voter-power))
    (map-set proposal-vote-counts {proposal-id: proposal-id, option: new-option} (+ new-count voter-power))
    
    (log-event "VOTE_CHANGED" "Vote updated")
    (ok new-option)
  )
)

;; Batch operations
(define-public (batch-set-voting-power (users (list 50 principal)) (powers (list 50 uint)))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (asserts! (is-eq (len users) (len powers)) ERR-INVALID-OPTION)
    (ok (map set-user-power users powers))
  )
)

(define-private (set-user-power (user principal) (power uint))
  (begin
    (map-set user-voting-power user power)
    true
  )
)

;; Analytics and reporting
(define-read-only (get-voting-analytics)
  {
    total-proposals: (var-get current-proposal-id),
    total-votes-cast: (var-get total-votes),
    voting-paused: (is-voting-paused),
    pause-reason: (var-get pause-reason),
    token-requirement: (var-get voting-token-requirement),
    admin-count: (var-get admin-count)
  }
)

(define-read-only (get-user-stats (user principal))
  {
    voting-power: (get-voting-power user),
    is-blacklisted: (is-blacklisted user),
    is-admin: (is-admin user)
  }
)

;; Emergency functions
(define-public (emergency-reset-proposal (proposal-id uint))
  (begin
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (asserts! (var-get voting-paused) ERR-VOTING-NOT-PAUSED) ;; Only during emergency pause
    
    (let (
      (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
    )
      (map-set proposals proposal-id (merge proposal {
        total-votes: u0,
        is-active: false
      }))
      
      ;; Clear vote counts (simplified - in production, you'd want to iterate through all options)
      (map-delete proposal-vote-counts {proposal-id: proposal-id, option: u1})
      (map-delete proposal-vote-counts {proposal-id: proposal-id, option: u2})
      (map-delete proposal-vote-counts {proposal-id: proposal-id, option: u3})
      (map-delete proposal-vote-counts {proposal-id: proposal-id, option: u4})
      (map-delete proposal-vote-counts {proposal-id: proposal-id, option: u5})
      
      (log-event "EMERGENCY_RESET" "Proposal reset during emergency")
      (ok true)
    )
  )
)

(define-public (emergency-shutdown)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set voting-paused true)
    (var-set pause-reason "EMERGENCY SHUTDOWN - CONTACT ADMIN")
    (log-event "EMERGENCY_SHUTDOWN" "Contract emergency shutdown activated")
    (ok true)
  )
)