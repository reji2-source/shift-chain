;; ShiftChain DAO Governance Contract
;; A dynamic governance platform with expertise-weighted voting

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-member (err u101))
(define-constant err-proposal-not-found (err u102))
(define-constant err-already-voted (err u103))
(define-constant err-voting-ended (err u104))
(define-constant err-insufficient-stake (err u105))
(define-constant err-invalid-domain (err u106))

;; Data Variables
(define-data-var proposal-counter uint u0)
(define-data-var min-stake-required uint u1000)
(define-data-var voting-period uint u144) ;; ~24 hours in blocks

;; Member Structure
(define-map members 
    principal 
    {
        stake-amount: uint,
        stake-timestamp: uint,
        technical-reputation: uint,
        financial-reputation: uint,
        community-reputation: uint,
        strategic-reputation: uint,
        total-votes: uint,
        is-active: bool
    }
)

;; Proposal Structure
(define-map proposals
    uint
    {
        proposer: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        proposal-type: (string-ascii 20), ;; "general", "technical", "treasury"
        votes-for: uint,
        votes-against: uint,
        total-voting-power: uint,
        start-block: uint,
        end-block: uint,
        executed: bool,
        passed: bool
    }
)

;; Vote Tracking
(define-map votes
    {proposal-id: uint, voter: principal}
    {
        vote: bool, ;; true for yes, false for no
        voting-power: uint,
        timestamp: uint
    }
)

;; Expertise Domains
(define-map expertise-domains
    principal
    {
        technical: uint,
        financial: uint,
        community: uint,
        strategic: uint
    }
)

;; Member Registration
(define-public (register-member (stake-amount uint))
    (let (
        (caller tx-sender)
        (current-block block-height)
    )
        (asserts! (>= stake-amount (var-get min-stake-required)) err-insufficient-stake)
        (map-set members caller {
            stake-amount: stake-amount,
            stake-timestamp: current-block,
            technical-reputation: u0,
            financial-reputation: u0,
            community-reputation: u0,
            strategic-reputation: u0,
            total-votes: u0,
            is-active: true
        })
        (map-set expertise-domains caller {
            technical: u10,
            financial: u10,
            community: u10,
            strategic: u10
        })
        (ok true)
    )
)

;; Update Member Reputation
(define-public (update-reputation (member principal) (domain (string-ascii 20)) (points uint))
    (let (
        (member-data (unwrap! (map-get? members member) err-not-member))
        (current-expertise (unwrap! (map-get? expertise-domains member) err-not-member))
    )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (if (is-eq domain "technical")
            (begin
                (map-set expertise-domains member 
                    (merge current-expertise {technical: (+ (get technical current-expertise) points)}))
                (ok true)
            )
            (if (is-eq domain "financial")
                (begin
                    (map-set expertise-domains member 
                        (merge current-expertise {financial: (+ (get financial current-expertise) points)}))
                    (ok true)
                )
                (if (is-eq domain "community")
                    (begin
                        (map-set expertise-domains member 
                            (merge current-expertise {community: (+ (get community current-expertise) points)}))
                        (ok true)
                    )
                    (if (is-eq domain "strategic")
                        (begin
                            (map-set expertise-domains member 
                                (merge current-expertise {strategic: (+ (get strategic current-expertise) points)}))
                            (ok true)
                        )
                        err-invalid-domain
                    )
                )
            )
        )
    )
)

;; Calculate Voting Power
(define-read-only (calculate-voting-power (member principal) (proposal-type (string-ascii 20)))
    (let (
        (member-data (unwrap! (map-get? members member) (ok u0)))
        (expertise (unwrap! (map-get? expertise-domains member) (ok u0)))
        (base-stake (get stake-amount member-data))
        (stake-duration (- block-height (get stake-timestamp member-data)))
        (temporal-multiplier (+ u100 (/ stake-duration u144))) ;; Bonus per day
        (expertise-weight 
            (if (is-eq proposal-type "technical")
                (get technical expertise)
                (if (is-eq proposal-type "treasury")
                    (get financial expertise)
                    (if (is-eq proposal-type "community")
                        (get community expertise)
                        (/ (+ (get technical expertise) 
                              (get financial expertise) 
                              (get community expertise) 
                              (get strategic expertise)) u4)
                    )
                )
            )
        )
    )
        (ok (* (* base-stake temporal-multiplier) expertise-weight))
    )
)

;; Create Proposal
(define-public (create-proposal 
    (title (string-ascii 100)) 
    (description (string-ascii 500)) 
    (proposal-type (string-ascii 20))
)
    (let (
        (proposal-id (+ (var-get proposal-counter) u1))
        (caller tx-sender)
        (current-block block-height)
        (end-block (+ current-block (var-get voting-period)))
    )
        (asserts! (is-some (map-get? members caller)) err-not-member)
        (map-set proposals proposal-id {
            proposer: caller,
            title: title,
            description: description,
            proposal-type: proposal-type,
            votes-for: u0,
            votes-against: u0,
            total-voting-power: u0,
            start-block: current-block,
            end-block: end-block,
            executed: false,
            passed: false
        })
        (var-set proposal-counter proposal-id)
        (ok proposal-id)
    )
)

;; Cast Vote
(define-public (cast-vote (proposal-id uint) (vote-choice bool))
    (let (
        (caller tx-sender)
        (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
        (current-block block-height)
        (voting-power-result (unwrap! (calculate-voting-power caller (get proposal-type proposal)) (err u999)))
        (existing-vote (map-get? votes {proposal-id: proposal-id, voter: caller}))
    )
        (asserts! (is-some (map-get? members caller)) err-not-member)
        (asserts! (is-none existing-vote) err-already-voted)
        (asserts! (<= current-block (get end-block proposal)) err-voting-ended)
        
        ;; Record the vote
        (map-set votes {proposal-id: proposal-id, voter: caller} {
            vote: vote-choice,
            voting-power: voting-power-result,
            timestamp: current-block
        })
        
        ;; Update proposal vote counts
        (map-set proposals proposal-id 
            (merge proposal {
                votes-for: (if vote-choice 
                              (+ (get votes-for proposal) voting-power-result) 
                              (get votes-for proposal)),
                votes-against: (if vote-choice 
                                  (get votes-against proposal) 
                                  (+ (get votes-against proposal) voting-power-result)),
                total-voting-power: (+ (get total-voting-power proposal) voting-power-result)
            })
        )
        
        ;; Update member vote count
        (let ((member-data (unwrap! (map-get? members caller) err-not-member)))
            (map-set members caller 
                (merge member-data {total-votes: (+ (get total-votes member-data) u1)}))
        )
        
        (ok true)
    )
)

;; Execute Proposal
(define-public (execute-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
        (current-block block-height)
        (total-for (get votes-for proposal))
        (total-against (get votes-against proposal))
        (passed (> total-for total-against))
    )
        (asserts! (> current-block (get end-block proposal)) err-voting-ended)
        (asserts! (not (get executed proposal)) (err u107))
        
        (map-set proposals proposal-id 
            (merge proposal {
                executed: true,
                passed: passed
            })
        )
        
        (ok passed)
    )
)

;; Read Functions
(define-read-only (get-member (member principal))
    (map-get? members member)
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (get-expertise (member principal))
    (map-get? expertise-domains member)
)

(define-read-only (get-proposal-count)
    (var-get proposal-counter)
)

;; Admin Functions
(define-public (set-min-stake (new-min uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set min-stake-required new-min)
        (ok true)
    )
)

(define-public (set-voting-period (new-period uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set voting-period new-period)
        (ok true)
    )
)