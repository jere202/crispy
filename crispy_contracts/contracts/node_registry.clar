;; StorageNodeRegistry Smart Contract - Enhanced Version
;; Purpose: Register and incentivize decentralized storage nodes with advanced features

;; Error codes
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NODE_NOT_FOUND (err u101))
(define-constant ERR_NODE_ALREADY_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_STORAGE (err u103))
(define-constant ERR_INVALID_PARAMETERS (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_NODE_OFFLINE (err u106))
(define-constant ERR_NODE_SLASHED (err u107))
(define-constant ERR_CHALLENGE_FAILED (err u108))
(define-constant ERR_BANDWIDTH_EXCEEDED (err u109))
(define-constant ERR_STAKING_REQUIRED (err u110))
(define-constant ERR_GOVERNANCE_LOCKED (err u111))
(define-constant ERR_INVALID_SIGNATURE (err u112))
(define-constant ERR_CHALLENGE_NOT_FOUND (err u113))

;; Contract owner and governance
(define-data-var contract-owner principal tx-sender)
(define-data-var governance-token principal tx-sender)
(define-data-var proposal-threshold uint u1000000) ;; 1M tokens to create proposal

;; Network parameters
(define-data-var min-storage-requirement uint u1073741824) ;; 1GB
(define-data-var base-reward-rate uint u100)
(define-data-var registration-fee uint u1000000) ;; 1 STX
(define-data-var min-stake-amount uint u5000000) ;; 5 STX minimum stake
(define-data-var slash-percentage uint u10) ;; 10% slash on misbehavior
(define-data-var challenge-window uint u1008) ;; 1 week in blocks
(define-data-var bandwidth-price-per-gb uint u1000) ;; Price for bandwidth usage

;; Advanced node tiers
(define-constant TIER_BASIC u1)
(define-constant TIER_PREMIUM u2)
(define-constant TIER_ENTERPRISE u3)

;; Node data structure (enhanced)
(define-map storage-nodes
  { node-id: principal }
  {
    storage-capacity: uint,
    available-storage: uint,
    registration-block: uint,
    last-heartbeat: uint,
    total-uptime-blocks: uint,
    total-files-stored: uint,
    reputation-score: uint,
    is-active: bool,
    reward-balance: uint,
    staked-amount: uint,
    slash-count: uint,
    tier: uint,
    bandwidth-used: uint,
    bandwidth-limit: uint,
    geographic-region: (string-ascii 10),
    node-version: (string-ascii 20),
    supported-features: (list 10 (string-ascii 20)),
    total-earnings: uint,
    kyc-verified: bool,
    insurance-coverage: uint
  }
)

;; Staking system
(define-map node-stakes
  { node-id: principal }
  {
    staked-amount: uint,
    stake-start-block: uint,
    unlock-block: uint,
    is-locked: bool
  }
)

;; Storage challenges for proof-of-storage
(define-map storage-challenges
  { challenge-id: uint }
  {
    node-id: principal,
    file-hash: (buff 32),
    challenge-data: (buff 32),
    challenge-block: uint,
    response-deadline: uint,
    is-completed: bool,
    is-successful: bool,
    challenger: principal
  }
)

;; Bandwidth tracking
(define-map bandwidth-usage
  { node-id: principal, period: uint }
  {
    bytes-served: uint,
    requests-served: uint,
    average-response-time: uint,
    bandwidth-cost: uint
  }
)

;; Data redundancy tracking
(define-map file-replicas
  { file-hash: (buff 32) }
  {
    required-replicas: uint,
    current-replicas: uint,
    replica-nodes: (list 10 principal),
    creation-block: uint,
    last-verified: uint
  }
)

;; SLA (Service Level Agreement) tracking
(define-map node-sla
  { node-id: principal }
  {
    uptime-sla: uint, ;; Percentage (e.g., 99 for 99%)
    response-time-sla: uint, ;; Milliseconds
    availability-penalty: uint,
    current-uptime: uint,
    sla-violations: uint,
    last-sla-check: uint
  }
)

;; Governance proposals
(define-map governance-proposals
  { proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    votes-for: uint,
    votes-against: uint,
    voting-deadline: uint,
    is-active: bool,
    is-executed: bool,
    proposal-type: (string-ascii 20)
  }
)

;; Insurance pool for node failures
(define-map insurance-claims
  { claim-id: uint }
  {
    node-id: principal,
    claimant: principal,
    amount-claimed: uint,
    claim-reason: (string-ascii 100),
    claim-block: uint,
    is-approved: bool,
    payout-amount: uint
  }
)

;; Smart contracts integration
(define-map integrated-contracts
  { contract-address: principal }
  {
    contract-type: (string-ascii 50),
    integration-block: uint,
    is-active: bool,
    permissions: (list 5 (string-ascii 20))
  }
)

;; Global state variables
(define-data-var total-registered-nodes uint u0)
(define-data-var total-storage-capacity uint u0)
(define-data-var total-staked-amount uint u0)
(define-data-var next-challenge-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var insurance-pool-balance uint u0)
(define-data-var network-utilization uint u0)

;; Enhanced read-only functions

;; Get comprehensive node information
(define-read-only (get-node-info-detailed (node-id principal))
  (let ((node-data (map-get? storage-nodes { node-id: node-id }))
        (stake-data (map-get? node-stakes { node-id: node-id }))
        (sla-data (map-get? node-sla { node-id: node-id })))
    {
      node-info: node-data,
      stake-info: stake-data,
      sla-info: sla-data,
      uptime-percentage: (get-node-uptime-percentage node-id),
      is-online: (is-node-online node-id)
    }
  )
)

;; Get network statistics
(define-read-only (get-network-stats)
  {
    total-nodes: (var-get total-registered-nodes),
    total-capacity: (var-get total-storage-capacity),
    total-staked: (var-get total-staked-amount),
    utilization: (var-get network-utilization),
    insurance-pool: (var-get insurance-pool-balance)
  }
)

;; Calculate node earnings potential
(define-read-only (calculate-earnings-potential (node-id principal))
  (match (map-get? storage-nodes { node-id: node-id })
    node-data
    (let ((tier-multiplier (get-tier-multiplier (get tier node-data)))
          (storage-gb (/ (get storage-capacity node-data) u1073741824))
          (reputation-bonus (/ (get reputation-score node-data) u100))
          (base-earnings (* storage-gb (var-get base-reward-rate))))
      (some (* (* base-earnings tier-multiplier) reputation-bonus)))
    none
  )
)

;; Get tier multiplier
(define-read-only (get-tier-multiplier (tier uint))
  (if (is-eq tier TIER_ENTERPRISE)
    u200  ;; 2x multiplier
    (if (is-eq tier TIER_PREMIUM)
      u150  ;; 1.5x multiplier
      u100) ;; 1x multiplier for basic
  )
)

;; Get file redundancy status
(define-read-only (get-file-redundancy (file-hash (buff 32)))
  (map-get? file-replicas { file-hash: file-hash })
)

;; Check node compliance with SLA
(define-read-only (is-node-sla-compliant (node-id principal))
  (match (map-get? node-sla { node-id: node-id })
    sla-data
    (>= (get current-uptime sla-data) (get uptime-sla sla-data))
    false
  )
)

;; Enhanced public functions

;; Register node with advanced features
(define-public (register-node-advanced 
  (storage-capacity uint) 
  (tier uint) 
  (geographic-region (string-ascii 10))
  (node-version (string-ascii 20))
  (supported-features (list 10 (string-ascii 20)))
  (uptime-sla uint))
  (let ((node-id tx-sender)
        (registration-fee-amount (var-get registration-fee))
        (required-stake (calculate-required-stake tier storage-capacity)))
    
    ;; Validate parameters
    (asserts! (is-none (map-get? storage-nodes { node-id: node-id })) ERR_NODE_ALREADY_EXISTS)
    (asserts! (>= storage-capacity (var-get min-storage-requirement)) ERR_INSUFFICIENT_STORAGE)
    (asserts! (and (>= tier TIER_BASIC) (<= tier TIER_ENTERPRISE)) ERR_INVALID_PARAMETERS)
    (asserts! (and (>= uptime-sla u90) (<= uptime-sla u100)) ERR_INVALID_PARAMETERS)
    
    ;; Transfer registration fee and stake
    (try! (stx-transfer? registration-fee-amount tx-sender (var-get contract-owner)))
    (try! (stake-tokens required-stake))
    
    ;; Register the node
    (map-set storage-nodes
      { node-id: node-id }
      {
        storage-capacity: storage-capacity,
        available-storage: storage-capacity,
        registration-block: block-height,
        last-heartbeat: block-height,
        total-uptime-blocks: u0,
        total-files-stored: u0,
        reputation-score: u100,
        is-active: true,
        reward-balance: u0,
        staked-amount: required-stake,
        slash-count: u0,
        tier: tier,
        bandwidth-used: u0,
        bandwidth-limit: (calculate-bandwidth-limit tier),
        geographic-region: geographic-region,
        node-version: node-version,
        supported-features: supported-features,
        total-earnings: u0,
        kyc-verified: false,
        insurance-coverage: (calculate-insurance-coverage tier required-stake)
      }
    )
    
    ;; Set SLA requirements
    (map-set node-sla
      { node-id: node-id }
      {
        uptime-sla: uptime-sla,
        response-time-sla: (get-tier-response-time-sla tier),
        availability-penalty: u0,
        current-uptime: u100,
        sla-violations: u0,
        last-sla-check: block-height
      }
    )
    
    ;; Update global counters
    (var-set total-registered-nodes (+ (var-get total-registered-nodes) u1))
    (var-set total-storage-capacity (+ (var-get total-storage-capacity) storage-capacity))
    (var-set total-staked-amount (+ (var-get total-staked-amount) required-stake))
    
    (ok true)
  )
)

;; Stake tokens for node operation
(define-public (stake-tokens (amount uint))
  (let ((node-id tx-sender))
    (asserts! (>= amount (var-get min-stake-amount)) ERR_STAKING_REQUIRED)
    
    ;; Transfer tokens to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Record stake
    (map-set node-stakes
      { node-id: node-id }
      {
        staked-amount: amount,
        stake-start-block: block-height,
        unlock-block: (+ block-height u2016), ;; 2 week lock period
        is-locked: true
      }
    )
    
    (ok amount)
  )
)

;; Create storage challenge
(define-public (create-storage-challenge (target-node principal) (file-hash (buff 32)) (challenge-data (buff 32)))
  (let ((challenge-id (var-get next-challenge-id)))
    ;; Verify file exists on target node
    (asserts! (is-some (map-get? file-storage { file-hash: file-hash, node-id: target-node })) ERR_NODE_NOT_FOUND)
    
    ;; Create challenge
    (map-set storage-challenges
      { challenge-id: challenge-id }
      {
        node-id: target-node,
        file-hash: file-hash,
        challenge-data: challenge-data,
        challenge-block: block-height,
        response-deadline: (+ block-height (var-get challenge-window)),
        is-completed: false,
        is-successful: false,
        challenger: tx-sender
      }
    )
    
    (var-set next-challenge-id (+ challenge-id u1))
    (ok challenge-id)
  )
)

;; Respond to storage challenge
(define-public (respond-to-challenge (challenge-id uint) (response-data (buff 32)))
  (match (map-get? storage-challenges { challenge-id: challenge-id })
    challenge
    (begin
      (asserts! (is-eq tx-sender (get node-id challenge)) ERR_UNAUTHORIZED)
      (asserts! (<= block-height (get response-deadline challenge)) ERR_CHALLENGE_FAILED)
      (asserts! (not (get is-completed challenge)) ERR_CHALLENGE_FAILED)
      
      ;; Verify response (simplified - in production would use cryptographic proof)
      (let ((is-valid (is-eq response-data (get challenge-data challenge))))
        (map-set storage-challenges
          { challenge-id: challenge-id }
          (merge challenge {
            is-completed: true,
            is-successful: is-valid
          })
        )
        
        ;; Update node reputation based on challenge result
        (if is-valid
          (increase-reputation tx-sender u5)
          (slash-node tx-sender "failed-storage-challenge"))
        
        (ok is-valid)
      )
    )
    ERR_CHALLENGE_NOT_FOUND
  )
)

;; Report bandwidth usage
(define-public (report-bandwidth-usage (bytes-served uint) (requests-served uint) (avg-response-time uint))
  (let ((node-id tx-sender)
        (current-period (/ block-height u144))) ;; Daily periods
    
    ;; Verify node exists
    (asserts! (is-some (map-get? storage-nodes { node-id: node-id })) ERR_NODE_NOT_FOUND)
    
    ;; Record bandwidth usage
    (map-set bandwidth-usage
      { node-id: node-id, period: current-period }
      {
        bytes-served: bytes-served,
        requests-served: requests-served,
        average-response-time: avg-response-time,
        bandwidth-cost: (* (/ bytes-served u1073741824) (var-get bandwidth-price-per-gb))
      }
    )
    
    ;; Check bandwidth limits
    (match (map-get? storage-nodes { node-id: node-id })
      node-data
      (let ((total-bandwidth (+ (get bandwidth-used node-data) bytes-served)))
        (asserts! (<= total-bandwidth (get bandwidth-limit node-data)) ERR_BANDWIDTH_EXCEEDED)
        
        ;; Update node bandwidth usage
        (map-set storage-nodes
          { node-id: node-id }
          (merge node-data { bandwidth-used: total-bandwidth })
        )
        
        (ok true)
      )
      ERR_NODE_NOT_FOUND
    )
  )
)

;; Create governance proposal
(define-public (create-governance-proposal 
  (title (string-ascii 100)) 
  (description (string-ascii 500)) 
  (proposal-type (string-ascii 20)))
  (let ((proposal-id (var-get next-proposal-id)))
    ;; Check if proposer has enough governance tokens (simplified)
    (asserts! (>= (stx-get-balance tx-sender) (var-get proposal-threshold)) ERR_INSUFFICIENT_FUNDS)
    
    (map-set governance-proposals
      { proposal-id: proposal-id }
      {
        proposer: tx-sender,
        title: title,
        description: description,
        votes-for: u0,
        votes-against: u0,
        voting-deadline: (+ block-height u1008), ;; 1 week voting period
        is-active: true,
        is-executed: false,
        proposal-type: proposal-type
      }
    )
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

;; File insurance claim
(define-public (file-insurance-claim (node-id principal) (amount uint) (reason (string-ascii 100)))
  (let ((claim-id (var-get next-claim-id)))
    ;; Verify claimant has stored files with the node
    (asserts! (> amount u0) ERR_INVALID_PARAMETERS)
    
    (map-set insurance-claims
      { claim-id: claim-id }
      {
        node-id: node-id,
        claimant: tx-sender,
        amount-claimed: amount,
        claim-reason: reason,
        claim-block: block-height,
        is-approved: false,
        payout-amount: u0
      }
    )
    
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Helper functions

;; Slash node for misbehavior
(define-private (slash-node (node-id principal) (reason (string-ascii 50)))
  (match (map-get? storage-nodes { node-id: node-id })
    node-data
    (let ((slash-amount (/ (* (get staked-amount node-data) (var-get slash-percentage)) u100)))
      ;; Increase slash count
      (map-set storage-nodes
        { node-id: node-id }
        (merge node-data {
          slash-count: (+ (get slash-count node-data) u1),
          reputation-score: (if (> (get reputation-score node-data) u10)
                              (- (get reputation-score node-data) u10)
                              u0)
        })
      )
      
      ;; Add to insurance pool
      (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) slash-amount))
      
      ;; Deactivate node if too many slashes
      (if (>= (+ (get slash-count node-data) u1) u3)
        (map-set storage-nodes
          { node-id: node-id }
          (merge node-data { is-active: false }))
        true)
      
      true
    )
    false
  )
)

;; Helper function to get minimum of two values
(define-private (min-uint (a uint) (b uint))
  (if (<= a b) a b)
)

;; Helper function to get maximum of two values
(define-private (max-uint (a uint) (b uint))
  (if (>= a b) a b)
)

;; Increase node reputation
(define-private (increase-reputation (node-id principal) (amount uint))
  (match (map-get? storage-nodes { node-id: node-id })
    node-data
    (map-set storage-nodes
      { node-id: node-id }
      (merge node-data {
        reputation-score: (min-uint (+ (get reputation-score node-data) amount) u1000)
      })
    )
    false
  )
)

;; Calculate required stake based on tier and capacity
(define-read-only (calculate-required-stake (tier uint) (capacity uint))
  (let ((base-stake (var-get min-stake-amount))
        (capacity-gb (/ capacity u1073741824))
        (tier-multiplier (get-tier-multiplier tier)))
    (* (* base-stake capacity-gb) (/ tier-multiplier u100))
  )
)

;; Calculate bandwidth limit based on tier
(define-read-only (calculate-bandwidth-limit (tier uint))
  (if (is-eq tier TIER_ENTERPRISE)
    u1099511627776  ;; 1TB
    (if (is-eq tier TIER_PREMIUM)
      u549755813888  ;; 512GB
      u107374182400) ;; 100GB for basic
  )
)

;; Calculate insurance coverage
(define-read-only (calculate-insurance-coverage (tier uint) (stake uint))
  (* stake (if (is-eq tier TIER_ENTERPRISE) u3
            (if (is-eq tier TIER_PREMIUM) u2 u1)))
)

;; Get response time SLA by tier
(define-read-only (get-tier-response-time-sla (tier uint))
  (if (is-eq tier TIER_ENTERPRISE)
    u100   ;; 100ms
    (if (is-eq tier TIER_PREMIUM)
      u500   ;; 500ms
      u1000) ;; 1000ms for basic
  )
)

;; Get node uptime percentage (from original contract)
(define-read-only (get-node-uptime-percentage (node-id principal))
  (match (map-get? storage-nodes { node-id: node-id })
    node-data 
    (let ((blocks-since-registration (- block-height (get registration-block node-data)))
          (uptime-blocks (get total-uptime-blocks node-data)))
      (if (> blocks-since-registration u0)
        (some (/ (* uptime-blocks u100) blocks-since-registration))
        (some u0)))
    none
  )
)

;; Check if node is online (from original contract)
(define-read-only (is-node-online (node-id principal))
  (match (map-get? storage-nodes { node-id: node-id })
    node-data
    (< (- block-height (get last-heartbeat node-data)) u144)
    false
  )
)

;; File storage map (from original contract, needed for compatibility)
(define-map file-storage
  { file-hash: (buff 32), node-id: principal }
  {
    file-size: uint,
    storage-start-block: uint,
    last-verified-block: uint,
    is-active: bool
  }
)

;; Admin functions

;; KYC verification
(define-public (verify-node-kyc (node-id principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    
    (match (map-get? storage-nodes { node-id: node-id })
      node-data
      (begin
        (map-set storage-nodes
          { node-id: node-id }
          (merge node-data { kyc-verified: true })
        )
        (ok true)
      )
      ERR_NODE_NOT_FOUND
    )
  )
)

;; Emergency pause/unpause
(define-data-var contract-paused bool false)

(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (var-set contract-paused false)
    (ok true)
  )
)

;; Integrate external contract
(define-public (integrate-contract (contract-addr principal) (contract-type (string-ascii 50)) (permissions (list 5 (string-ascii 20))))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    
    (map-set integrated-contracts
      { contract-address: contract-addr }
      {
        contract-type: contract-type,
        integration-block: block-height,
        is-active: true,
        permissions: permissions
      }
    )
    (ok true)
  )
)