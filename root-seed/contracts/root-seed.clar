;; RootSeed - Hierarchical Delegation Blockchain Platform
;; A governance system with merit-based delegation and decay mechanisms

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-input (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-delegation-expired (err u105))

;; Decay period in blocks (approximately 30 days at 10 min/block)
(define-constant delegation-decay-period u4320)

;; Data Variables
(define-data-var proposal-nonce uint u0)

;; Data Maps

;; Store delegation relationships
(define-map delegations
    {delegator: principal, domain: (string-ascii 50)}
    {
        delegate: principal,
        delegated-at: uint,
        last-renewed: uint,
        power-multiplier: uint ;; Base 100, so 100 = 1x, 150 = 1.5x
    }
)

;; Store user voting power
(define-map voting-power
    principal
    {
        base-power: uint,
        delegated-power: uint,
        total-power: uint
    }
)

;; Store proposals
(define-map proposals
    uint
    {
        creator: principal,
        domain: (string-ascii 50),
        title: (string-utf8 200),
        description: (string-utf8 1000),
        created-at: uint,
        voting-ends: uint,
        votes-for: uint,
        votes-against: uint,
        executed: bool,
        passed: bool
    }
)

;; Track votes on proposals
(define-map proposal-votes
    {proposal-id: uint, voter: principal}
    {
        vote: bool, ;; true = for, false = against
        voting-power: uint,
        voted-at: uint
    }
)

;; Store domain specialists
(define-map domain-specialists
    {domain: (string-ascii 50), specialist: principal}
    {
        reputation-score: uint,
        registered-at: uint,
        active: bool
    }
)

;; Minority protection weights by group size
(define-map minority-weights
    uint ;; group size
    uint ;; amplification multiplier (base 100)
)

;; Read-only functions

(define-read-only (get-delegation (delegator principal) (domain (string-ascii 50)))
    (map-get? delegations {delegator: delegator, domain: domain})
)

(define-read-only (get-voting-power (user principal))
    (default-to 
        {base-power: u0, delegated-power: u0, total-power: u0}
        (map-get? voting-power user)
    )
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? proposal-votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (get-domain-specialist (domain (string-ascii 50)) (specialist principal))
    (map-get? domain-specialists {domain: domain, specialist: specialist})
)

(define-read-only (calculate-delegation-decay (delegated-at uint) (last-renewed uint))
    (let
        (
            (current-block block-height)
            (blocks-since-renewal (- current-block last-renewed))
            (decay-factor (if (>= blocks-since-renewal delegation-decay-period)
                u0
                (- u100 (/ (* blocks-since-renewal u100) delegation-decay-period))
            ))
        )
        decay-factor
    )
)

(define-read-only (get-effective-voting-power (user principal) (domain (string-ascii 50)))
    (let
        (
            (power-data (get-voting-power user))
            (delegation-data (get-delegation user domain))
        )
        (match delegation-data
            delegation
            (let
                (
                    (decay (calculate-delegation-decay 
                        (get delegated-at delegation) 
                        (get last-renewed delegation)
                    ))
                    (base (get total-power power-data))
                )
                (/ (* base decay) u100)
            )
            (get total-power power-data)
        )
    )
)

;; Public functions

(define-public (initialize-voting-power (amount uint))
    (let
        (
            (current-power (get-voting-power tx-sender))
        )
        (ok (map-set voting-power
            tx-sender
            {
                base-power: amount,
                delegated-power: (get delegated-power current-power),
                total-power: (+ amount (get delegated-power current-power))
            }
        ))
    )
)

(define-public (delegate-vote (delegate principal) (domain (string-ascii 50)) (multiplier uint))
    (begin
        (asserts! (not (is-eq tx-sender delegate)) err-invalid-input)
        (asserts! (and (>= multiplier u50) (<= multiplier u200)) err-invalid-input)
        (ok (map-set delegations
            {delegator: tx-sender, domain: domain}
            {
                delegate: delegate,
                delegated-at: block-height,
                last-renewed: block-height,
                power-multiplier: multiplier
            }
        ))
    )
)

(define-public (renew-delegation (domain (string-ascii 50)))
    (let
        (
            (delegation-data (get-delegation tx-sender domain))
        )
        (match delegation-data
            delegation
            (ok (map-set delegations
                {delegator: tx-sender, domain: domain}
                (merge delegation {last-renewed: block-height})
            ))
            err-not-found
        )
    )
)

(define-public (revoke-delegation (domain (string-ascii 50)))
    (ok (map-delete delegations {delegator: tx-sender, domain: domain}))
)

(define-public (register-as-specialist (domain (string-ascii 50)) (initial-reputation uint))
    (begin
        (asserts! (<= initial-reputation u100) err-invalid-input)
        (ok (map-set domain-specialists
            {domain: domain, specialist: tx-sender}
            {
                reputation-score: initial-reputation,
                registered-at: block-height,
                active: true
            }
        ))
    )
)

(define-public (create-proposal 
    (domain (string-ascii 50))
    (title (string-utf8 200))
    (description (string-utf8 1000))
    (voting-duration uint))
    (let
        (
            (proposal-id (var-get proposal-nonce))
        )
        (map-set proposals
            proposal-id
            {
                creator: tx-sender,
                domain: domain,
                title: title,
                description: description,
                created-at: block-height,
                voting-ends: (+ block-height voting-duration),
                votes-for: u0,
                votes-against: u0,
                executed: false,
                passed: false
            }
        )
        (var-set proposal-nonce (+ proposal-id u1))
        (ok proposal-id)
    )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
    (let
        (
            (proposal (unwrap! (get-proposal proposal-id) err-not-found))
            (voter-power (get total-power (get-voting-power tx-sender)))
            (existing-vote (get-vote proposal-id tx-sender))
        )
        (asserts! (< block-height (get voting-ends proposal)) err-unauthorized)
        (asserts! (is-none existing-vote) err-already-exists)
        (asserts! (> voter-power u0) err-invalid-input)
        
        (map-set proposal-votes
            {proposal-id: proposal-id, voter: tx-sender}
            {
                vote: vote-for,
                voting-power: voter-power,
                voted-at: block-height
            }
        )
        
        (if vote-for
            (map-set proposals
                proposal-id
                (merge proposal {votes-for: (+ (get votes-for proposal) voter-power)})
            )
            (map-set proposals
                proposal-id
                (merge proposal {votes-against: (+ (get votes-against proposal) voter-power)})
            )
        )
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let
        (
            (proposal (unwrap! (get-proposal proposal-id) err-not-found))
        )
        (asserts! (>= block-height (get voting-ends proposal)) err-unauthorized)
        (asserts! (not (get executed proposal)) err-already-exists)
        
        (let
            (
                (passed (> (get votes-for proposal) (get votes-against proposal)))
            )
            (ok (map-set proposals
                proposal-id
                (merge proposal {executed: true, passed: passed})
            ))
        )
    )
)

(define-public (update-reputation (domain (string-ascii 50)) (specialist principal) (new-score uint))
    (let
        (
            (specialist-data (unwrap! (get-domain-specialist domain specialist) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-score u100) err-invalid-input)
        (ok (map-set domain-specialists
            {domain: domain, specialist: specialist}
            (merge specialist-data {reputation-score: new-score})
        ))
    )
)

(define-public (set-minority-weight (group-size uint) (amplification uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (and (>= amplification u100) (<= amplification u300)) err-invalid-input)
        (ok (map-set minority-weights group-size amplification))
    )
)