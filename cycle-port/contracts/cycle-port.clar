;; MicroMember - Progressive DAO Governance with Micro-Credentials

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)

(define-constant ERR-NOT-AUTHORIZED         (err u100))
(define-constant ERR-ALREADY-EXISTS         (err u101))
(define-constant ERR-NOT-FOUND              (err u102))
(define-constant ERR-INVALID-PARAM          (err u103))
(define-constant ERR-INSUFFICIENT-STAKE     (err u104))
(define-constant ERR-PROPOSAL-CLOSED        (err u105))
(define-constant ERR-ALREADY-VOTED          (err u106))
(define-constant ERR-MILESTONE-NOT-READY    (err u107))
(define-constant ERR-DISPUTE-ACTIVE         (err u108))
(define-constant ERR-INSUFFICIENT-BALANCE   (err u109))

;; Governance tracks
(define-constant TRACK-TECHNICAL   u1)
(define-constant TRACK-MARKETING   u2)
(define-constant TRACK-TREASURY    u3)
(define-constant TRACK-GENERAL     u4)

;; Proposal statuses
(define-constant STATUS-ACTIVE     u1)
(define-constant STATUS-PASSED     u2)
(define-constant STATUS-REJECTED   u3)
(define-constant STATUS-EXECUTED   u4)
(define-constant STATUS-DISPUTED   u5)

;; Member privilege levels (onboarding pipeline)
(define-constant LEVEL-OBSERVER    u0)
(define-constant LEVEL-CONTRIBUTOR u1)
(define-constant LEVEL-MEMBER      u2)
(define-constant LEVEL-STEWARD     u3)

;; Minimum blocks for voting period (~1 day at ~144 blocks/day)
(define-constant MIN-VOTING-PERIOD u144)

;; Minimum stake required to submit a proposal
(define-constant MIN-PROPOSAL-STAKE u1000000) ;; 1 STX in uSTX

;; ============================================================
;; DATA MAPS AND VARS
;; ============================================================

;; Tracks next IDs
(define-data-var next-member-id    uint u1)
(define-data-var next-proposal-id  uint u1)
(define-data-var next-credential-id uint u1)
(define-data-var next-dispute-id   uint u1)

;; Members
(define-map members
  { address: principal }
  {
    member-id:       uint,
    level:           uint,
    reputation:      uint,
    staked-amount:   uint,
    joined-at:       uint,
    active:          bool
  }
)

;; Credentials issued to members for a specific track
(define-map credentials
  { credential-id: uint }
  {
    holder:      principal,
    track:       uint,
    score:       uint,   ;; 0-100 proficiency score
    issued-at:   uint,
    issuer:      principal,
    valid:       bool
  }
)

;; Index: member -> track -> credential-id (latest valid credential)
(define-map member-track-credential
  { address: principal, track: uint }
  { credential-id: uint }
)

;; Proposals
(define-map proposals
  { proposal-id: uint }
  {
    proposer:        principal,
    track:           uint,
    title:           (string-ascii 100),
    description:     (string-ascii 500),
    amount:          uint,         ;; total STX requested (uSTX)
    milestones:      uint,         ;; number of milestones
    status:          uint,
    yes-votes:       uint,         ;; raw quadratic vote weight sum
    no-votes:        uint,
    created-at:      uint,
    voting-ends-at:  uint,
    stake-amount:    uint          ;; proposer stake locked
  }
)

;; Vote records (prevent double voting)
(define-map votes
  { proposal-id: uint, voter: principal }
  {
    vote-weight: uint,
    in-favor:    bool
  }
)

;; Escrow per proposal per milestone
(define-map milestone-data
  { proposal-id: uint, milestone-index: uint }
  {
    amount:       uint,
    released:     bool,
    approved-by:  (optional principal)
  }
)

;; Staked STX balances (held by contract)
(define-map stake-balances
  { address: principal }
  { amount: uint }
)

;; Disputes
(define-map disputes
  { dispute-id: uint }
  {
    proposal-id:  uint,
    raised-by:    principal,
    reason:       (string-ascii 300),
    resolved:     bool,
    outcome:      (optional bool)  ;; true = upheld, false = dismissed
  }
)

;; ============================================================
;; PRIVATE HELPERS
;; ============================================================

;; Integer square root (for quadratic voting weight)
(define-private (isqrt (n uint))
  (if (is-eq n u0)
    u0
    (let (
      (x1 (+ (/ n u2) u1))
      (x2 (/ (+ (/ n x1) x1) u2))
    )
      ;; Two Newton iterations - sufficient for values up to ~10^12
      (let (
        (x3 (/ (+ (/ n x2) x2) u2))
        (x4 (/ (+ (/ n x3) x3) u2))
      )
        (if (<= x4 x3) x4 x3)
      )
    )
  )
)

;; Get credential score for a member in a track (0 if none)
(define-private (get-credential-score (address principal) (track uint))
  (match (map-get? member-track-credential { address: address, track: track })
    cred-ref
      (match (map-get? credentials { credential-id: (get credential-id cred-ref) })
        cred (if (get valid cred) (get score cred) u0)
        u0)
    u0)
)

;; Compute voting weight: quadratic on reputation, scaled by credential score
;; weight = sqrt(reputation) * (1 + credential_score / 100)
;; To keep integer math: weight = sqrt(reputation) * (100 + credential_score) / 100
(define-private (compute-vote-weight (address principal) (track uint))
  (match (map-get? members { address: address })
    m
      (let (
        (base-weight  (isqrt (get reputation m)))
        (cred-score   (get-credential-score address track))
        (multiplier   (+ u100 cred-score))
      )
        (/ (* base-weight multiplier) u100)
      )
    u0)
)

;; Check if principal is a member at or above a given level
(define-private (is-member-at-level (address principal) (min-level uint))
  (match (map-get? members { address: address })
    m (and (get active m) (>= (get level m) min-level))
    false)
)

;; ============================================================
;; MEMBER MANAGEMENT
;; ============================================================

;; Register as an observer (open entry point)
(define-public (register-member)
  (let ((caller tx-sender))
    (asserts! (is-none (map-get? members { address: caller })) ERR-ALREADY-EXISTS)
    (map-set members
      { address: caller }
      {
        member-id:     (var-get next-member-id),
        level:         LEVEL-OBSERVER,
        reputation:    u10,
        staked-amount: u0,
        joined-at:     block-height,
        active:        true
      }
    )
    (var-set next-member-id (+ (var-get next-member-id) u1))
    (ok true)
  )
)

;; Admin: promote a member to a higher privilege level
(define-public (promote-member (address principal) (new-level uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= new-level u0) (<= new-level LEVEL-STEWARD)) ERR-INVALID-PARAM)
    (match (map-get? members { address: address })
      m (begin
          (map-set members
            { address: address }
            (merge m { level: new-level })
          )
          (ok true)
        )
      ERR-NOT-FOUND)
  )
)

;; ============================================================
;; CREDENTIAL SYSTEM
;; ============================================================

;; Issue a credential (must be STEWARD or CONTRACT-OWNER)
(define-public (issue-credential
    (recipient principal)
    (track      uint)
    (score      uint))
  (let ((caller tx-sender))
    (asserts!
      (or (is-eq caller CONTRACT-OWNER) (is-member-at-level caller LEVEL-STEWARD))
      ERR-NOT-AUTHORIZED)
    (asserts! (and (>= score u0) (<= score u100)) ERR-INVALID-PARAM)
    (asserts! (is-some (map-get? members { address: recipient })) ERR-NOT-FOUND)
    (let ((cred-id (var-get next-credential-id)))
      ;; Invalidate previous credential for this track if any
      (match (map-get? member-track-credential { address: recipient, track: track })
        old-ref
          (match (map-get? credentials { credential-id: (get credential-id old-ref) })
            old-cred
              (map-set credentials
                { credential-id: (get credential-id old-ref) }
                (merge old-cred { valid: false })
              )
            true)
        true)
      ;; Write new credential
      (map-set credentials
        { credential-id: cred-id }
        {
          holder:    recipient,
          track:     track,
          score:     score,
          issued-at: block-height,
          issuer:    caller,
          valid:     true
        }
      )
      (map-set member-track-credential
        { address: recipient, track: track }
        { credential-id: cred-id }
      )
      ;; Increase recipient reputation proportional to score
      (match (map-get? members { address: recipient })
        m (map-set members
            { address: recipient }
            (merge m { reputation: (+ (get reputation m) (/ score u10)) })
          )
        true)
      (var-set next-credential-id (+ cred-id u1))
      (ok cred-id)
    )
  )
)

;; ============================================================
;; REPUTATION STAKING
;; ============================================================

;; Stake STX to the contract (increases staked-amount, held in escrow)
(define-public (stake (amount uint))
  (let ((caller tx-sender))
    (asserts! (> amount u0) ERR-INVALID-PARAM)
    (asserts! (is-member-at-level caller LEVEL-OBSERVER) ERR-NOT-AUTHORIZED)
    (try! (stx-transfer? amount caller (as-contract tx-sender)))
    (match (map-get? stake-balances { address: caller })
      existing
        (map-set stake-balances
          { address: caller }
          { amount: (+ (get amount existing) amount) }
        )
      (map-set stake-balances
        { address: caller }
        { amount: amount }
      )
    )
    ;; Boost reputation proportional to stake (1 rep per 100000 uSTX)
    (match (map-get? members { address: caller })
      m (map-set members
          { address: caller }
          (merge m {
            staked-amount: (+ (get staked-amount m) amount),
            reputation:    (+ (get reputation m) (/ amount u100000))
          })
        )
      true)
    (ok true)
  )
)

;; Unstake STX (decreases reputation proportionally)
(define-public (unstake (amount uint))
  (let ((caller tx-sender))
    (asserts! (> amount u0) ERR-INVALID-PARAM)
    (match (map-get? stake-balances { address: caller })
      bal
        (begin
          (asserts! (>= (get amount bal) amount) ERR-INSUFFICIENT-BALANCE)
          (try! (as-contract (stx-transfer? amount tx-sender caller)))
          (map-set stake-balances
            { address: caller }
            { amount: (- (get amount bal) amount) }
          )
          (match (map-get? members { address: caller })
            m
              (let ((rep-loss (/ amount u100000)))
                (map-set members
                  { address: caller }
                  (merge m {
                    staked-amount: (- (get staked-amount m) amount),
                    reputation:    (if (>= (get reputation m) rep-loss)
                                    (- (get reputation m) rep-loss)
                                    u0)
                  })
                )
              )
            true)
          (ok true)
        )
      ERR-NOT-FOUND)
  )
)

;; ============================================================
;; PROPOSALS
;; ============================================================

;; Submit a proposal with STX stake and milestone count
(define-public (submit-proposal
    (track           uint)
    (title           (string-ascii 100))
    (description     (string-ascii 500))
    (amount          uint)
    (milestones      uint)
    (voting-period   uint))
  (let (
    (caller      tx-sender)
    (proposal-id (var-get next-proposal-id))
  )
    (asserts! (is-member-at-level caller LEVEL-CONTRIBUTOR) ERR-NOT-AUTHORIZED)
    (asserts! (>= voting-period MIN-VOTING-PERIOD) ERR-INVALID-PARAM)
    (asserts! (> milestones u0) ERR-INVALID-PARAM)
    (asserts! (> amount u0) ERR-INVALID-PARAM)
    ;; Proposer must lock stake
    (try! (stx-transfer? MIN-PROPOSAL-STAKE caller (as-contract tx-sender)))
    ;; Lock proposal funds in escrow
    (try! (stx-transfer? amount caller (as-contract tx-sender)))
    (map-set proposals
      { proposal-id: proposal-id }
      {
        proposer:       caller,
        track:          track,
        title:          title,
        description:    description,
        amount:         amount,
        milestones:     milestones,
        status:         STATUS-ACTIVE,
        yes-votes:      u0,
        no-votes:       u0,
        created-at:     block-height,
        voting-ends-at: (+ block-height voting-period),
        stake-amount:   MIN-PROPOSAL-STAKE
      }
    )
    ;; Initialize milestones with equal amounts
    (let ((milestone-amount (/ amount milestones)))
      (map-set milestone-data
        { proposal-id: proposal-id, milestone-index: u0 }
        { amount: milestone-amount, released: false, approved-by: none }
      )
    )
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

;; Cast a vote (quadratic weight auto-calculated)
(define-public (vote (proposal-id uint) (in-favor bool))
  (let ((caller tx-sender))
    (asserts! (is-member-at-level caller LEVEL-CONTRIBUTOR) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: caller })) ERR-ALREADY-VOTED)
    (match (map-get? proposals { proposal-id: proposal-id })
      proposal
        (begin
          (asserts! (is-eq (get status proposal) STATUS-ACTIVE) ERR-PROPOSAL-CLOSED)
          (asserts! (<= block-height (get voting-ends-at proposal)) ERR-PROPOSAL-CLOSED)
          (let ((weight (compute-vote-weight caller (get track proposal))))
            (map-set votes
              { proposal-id: proposal-id, voter: caller }
              { vote-weight: weight, in-favor: in-favor }
            )
            (if in-favor
              (map-set proposals
                { proposal-id: proposal-id }
                (merge proposal { yes-votes: (+ (get yes-votes proposal) weight) })
              )
              (map-set proposals
                { proposal-id: proposal-id }
                (merge proposal { no-votes: (+ (get no-votes proposal) weight) })
              )
            )
            (ok weight)
          )
        )
      ERR-NOT-FOUND)
  )
)

;; Finalize proposal after voting period ends
(define-public (finalize-proposal (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal
      (begin
        (asserts! (is-eq (get status proposal) STATUS-ACTIVE) ERR-PROPOSAL-CLOSED)
        (asserts! (> block-height (get voting-ends-at proposal)) ERR-PROPOSAL-CLOSED)
        (let (
          (new-status (if (> (get yes-votes proposal) (get no-votes proposal))
                        STATUS-PASSED
                        STATUS-REJECTED))
        )
          ;; If rejected, return funds and stake to proposer
          (if (is-eq new-status STATUS-REJECTED)
            (begin
              (try! (as-contract (stx-transfer?
                (+ (get amount proposal) (get stake-amount proposal))
                tx-sender
                (get proposer proposal))))
              true)
            ;; If passed, return only the proposal stake (funds stay in escrow)
            (begin
              (try! (as-contract (stx-transfer?
                (get stake-amount proposal)
                tx-sender
                (get proposer proposal))))
              true)
          )
          (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal { status: new-status })
          )
          (ok new-status)
        )
      )
    ERR-NOT-FOUND)
)

;; ============================================================
;; MILESTONE RELEASE (multi-sig committee approval)
;; ============================================================

;; Steward approves a milestone and releases its portion of funds
(define-public (approve-milestone
    (proposal-id     uint)
    (milestone-index uint)
    (recipient       principal))
  (let ((caller tx-sender))
    (asserts! (is-member-at-level caller LEVEL-STEWARD) ERR-NOT-AUTHORIZED)
    (match (map-get? proposals { proposal-id: proposal-id })
      proposal
        (begin
          (asserts! (is-eq (get status proposal) STATUS-PASSED) ERR-PROPOSAL-CLOSED)
          (match (map-get? milestone-data { proposal-id: proposal-id, milestone-index: milestone-index })
            ms
              (begin
                (asserts! (not (get released ms)) ERR-MILESTONE-NOT-READY)
                (try! (as-contract (stx-transfer? (get amount ms) tx-sender recipient)))
                (map-set milestone-data
                  { proposal-id: proposal-id, milestone-index: milestone-index }
                  (merge ms { released: true, approved-by: (some caller) })
                )
                ;; If all milestones released, mark executed
                (if (is-eq milestone-index (- (get milestones proposal) u1))
                  (map-set proposals
                    { proposal-id: proposal-id }
                    (merge proposal { status: STATUS-EXECUTED })
                  )
                  true)
                (ok true)
              )
            ERR-NOT-FOUND)
        )
      ERR-NOT-FOUND)
  )
)

;; ============================================================
;; DISPUTE RESOLUTION
;; ============================================================

;; Raise a dispute against an active or passed proposal
(define-public (raise-dispute
    (proposal-id uint)
    (reason      (string-ascii 300)))
  (let ((caller tx-sender))
    (asserts! (is-member-at-level caller LEVEL-MEMBER) ERR-NOT-AUTHORIZED)
    (match (map-get? proposals { proposal-id: proposal-id })
      proposal
        (begin
          (asserts!
            (or (is-eq (get status proposal) STATUS-ACTIVE)
                (is-eq (get status proposal) STATUS-PASSED))
            ERR-PROPOSAL-CLOSED)
          (let ((dispute-id (var-get next-dispute-id)))
            (map-set disputes
              { dispute-id: dispute-id }
              {
                proposal-id: proposal-id,
                raised-by:   caller,
                reason:      reason,
                resolved:    false,
                outcome:     none
              }
            )
            ;; Freeze proposal
            (map-set proposals
              { proposal-id: proposal-id }
              (merge proposal { status: STATUS-DISPUTED })
            )
            (var-set next-dispute-id (+ dispute-id u1))
            (ok dispute-id)
          )
        )
      ERR-NOT-FOUND)
  )
)

;; Resolve a dispute (only CONTRACT-OWNER for simplicity;
;; production should use a randomly selected expert panel)
(define-public (resolve-dispute
    (dispute-id  uint)
    (upheld      bool)
    (proposal-id uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (match (map-get? disputes { dispute-id: dispute-id })
      dispute
        (begin
          (asserts! (not (get resolved dispute)) ERR-PROPOSAL-CLOSED)
          (map-set disputes
            { dispute-id: dispute-id }
            (merge dispute { resolved: true, outcome: (some upheld) })
          )
          (match (map-get? proposals { proposal-id: proposal-id })
            proposal
              (begin
                ;; If upheld, reject the proposal and refund escrow
                (if upheld
                  (begin
                    (try! (as-contract (stx-transfer?
                      (get amount proposal)
                      tx-sender
                      (get proposer proposal))))
                    (map-set proposals
                      { proposal-id: proposal-id }
                      (merge proposal { status: STATUS-REJECTED })
                    )
                  )
                  ;; If dismissed, restore previous status (PASSED)
                  (map-set proposals
                    { proposal-id: proposal-id }
                    (merge proposal { status: STATUS-PASSED })
                  )
                )
                (ok true)
              )
            ERR-NOT-FOUND)
        )
      ERR-NOT-FOUND)
  )
)

;; ============================================================
;; READ-ONLY VIEWS
;; ============================================================

(define-read-only (get-member (address principal))
  (map-get? members { address: address })
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-credential (credential-id uint))
  (map-get? credentials { credential-id: credential-id })
)

(define-read-only (get-member-credential-for-track (address principal) (track uint))
  (match (map-get? member-track-credential { address: address, track: track })
    ref (map-get? credentials { credential-id: (get credential-id ref) })
    none)
)

(define-read-only (get-vote-weight (address principal) (track uint))
  (ok (compute-vote-weight address track))
)

(define-read-only (get-milestone (proposal-id uint) (milestone-index uint))
  (map-get? milestone-data { proposal-id: proposal-id, milestone-index: milestone-index })
)

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

(define-read-only (get-stake-balance (address principal))
  (default-to { amount: u0 } (map-get? stake-balances { address: address }))
)
