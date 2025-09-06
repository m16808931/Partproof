(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PART_NOT_FOUND (err u101))
(define-constant ERR_PART_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_MANUFACTURER (err u103))
(define-constant ERR_PART_ALREADY_VERIFIED (err u104))
(define-constant ERR_VERIFICATION_FAILED (err u105))
(define-constant ERR_WARRANTY_NOT_FOUND (err u106))
(define-constant ERR_WARRANTY_EXPIRED (err u107))
(define-constant ERR_CLAIM_NOT_FOUND (err u108))
(define-constant ERR_CLAIM_ALREADY_PROCESSED (err u109))
(define-constant ERR_INVALID_CLAIM_STATUS (err u110))
(define-constant ERR_INVALID_WARRANTY_DURATION (err u111))
(define-constant ERR_CLAIM_ALREADY_EXISTS (err u112))
(define-constant ERR_RATING_NOT_FOUND (err u113))
(define-constant ERR_INVALID_RATING_SCORE (err u114))
(define-constant ERR_RATING_ALREADY_EXISTS (err u115))
(define-constant ERR_NOT_PART_OWNER (err u116))
(define-constant ERR_RATING_TOO_EARLY (err u117))
(define-constant ERR_INVALID_RATING_CATEGORY (err u118))

(define-data-var next-part-id uint u1)
(define-data-var next-warranty-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var next-rating-id uint u1)

(define-map manufacturers
    principal
    {
        name: (string-ascii 50),
        verified: bool,
        registration-block: uint
    }
)

(define-map parts
    uint
    {
        manufacturer: principal,
        part-number: (string-ascii 50),
        description: (string-ascii 200),
        batch-id: (string-ascii 30),
        manufacture-date: uint,
        verification-hash: (buff 32),
        verified: bool,
        created-block: uint
    }
)

(define-map part-ownership
    uint
    {
        current-owner: principal,
        previous-owners: (list 10 principal),
        transfer-count: uint
    }
)

(define-map manufacturer-parts
    principal
    (list 1000 uint)
)

(define-map part-verifications
    uint
    {
        verifier: principal,
        verification-date: uint,
        authenticity-score: uint,
        notes: (string-ascii 100)
    }
)

(define-map part-warranties
    uint
    {
        warranty-id: uint,
        part-id: uint,
        manufacturer: principal,
        warranty-type: (string-ascii 50),
        duration-blocks: uint,
        coverage-description: (string-ascii 200),
        terms-conditions: (string-ascii 500),
        start-block: uint,
        end-block: uint,
        active: bool,
        created-block: uint
    }
)

(define-map warranty-claims
    uint
    {
        claim-id: uint,
        warranty-id: uint,
        part-id: uint,
        claimant: principal,
        claim-type: (string-ascii 50),
        description: (string-ascii 300),
        evidence-hash: (buff 32),
        claim-date: uint,
        status: (string-ascii 20),
        resolution-notes: (string-ascii 200),
        approved-by: (optional principal),
        processing-date: (optional uint),
        settlement-amount: (optional uint)
    }
)

(define-map part-warranty-lookup
    uint
    uint
)

(define-map manufacturer-warranty-claims
    principal
    (list 500 uint)
)

(define-map part-ratings
    uint
    {
        rating-id: uint,
        part-id: uint,
        rater: principal,
        overall-score: uint,
        durability-score: uint,
        performance-score: uint,
        value-score: uint,
        category: (string-ascii 30),
        review-text: (string-ascii 300),
        usage-duration: uint,
        verified: bool,
        rating-date: uint,
        helpful-votes: uint,
        total-votes: uint
    }
)

(define-map part-rating-aggregates
    uint
    {
        total-ratings: uint,
        average-overall: uint,
        average-durability: uint,
        average-performance: uint,
        average-value: uint,
        total-score-sum: uint,
        last-updated: uint,
        verified-ratings-count: uint
    }
)

(define-map manufacturer-rating-summary
    principal
    {
        total-parts-rated: uint,
        total-ratings: uint,
        average-rating: uint,
        last-updated: uint,
        reputation-score: uint
    }
)

(define-map user-part-ratings
    { user: principal, part-id: uint }
    uint
)

(define-map rating-helpfulness
    { rating-id: uint, voter: principal }
    bool
)

(define-public (register-manufacturer (name (string-ascii 50)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (ok (map-set manufacturers tx-sender {
            name: name,
            verified: true,
            registration-block: stacks-block-height
        }))
    )
)

(define-public (register-part 
    (part-number (string-ascii 50))
    (description (string-ascii 200))
    (batch-id (string-ascii 30))
    (manufacture-date uint)
    (verification-hash (buff 32)))
    (let
        (
            (part-id (var-get next-part-id))
            (manufacturer-info (map-get? manufacturers tx-sender))
        )
        (asserts! (is-some manufacturer-info) ERR_INVALID_MANUFACTURER)
        (asserts! (get verified (unwrap-panic manufacturer-info)) ERR_INVALID_MANUFACTURER)
        (asserts! (is-none (map-get? parts part-id)) ERR_PART_ALREADY_EXISTS)
        
        (map-set parts part-id {
            manufacturer: tx-sender,
            part-number: part-number,
            description: description,
            batch-id: batch-id,
            manufacture-date: manufacture-date,
            verification-hash: verification-hash,
            verified: false,
            created-block: stacks-block-height
        })
        
        (map-set part-ownership part-id {
            current-owner: tx-sender,
            previous-owners: (list),
            transfer-count: u0
        })
        
        (let ((current-parts (default-to (list) (map-get? manufacturer-parts tx-sender))))
            (map-set manufacturer-parts tx-sender (unwrap-panic (as-max-len? (append current-parts part-id) u1000)))
        )
        
        (var-set next-part-id (+ part-id u1))
        (ok part-id)
    )
)

(define-public (verify-part (part-id uint) (verification-hash (buff 32)) (authenticity-score uint) (notes (string-ascii 100)))
    (let
        (
            (part-info (map-get? parts part-id))
        )
        (asserts! (is-some part-info) ERR_PART_NOT_FOUND)
        (let ((part-data (unwrap-panic part-info)))
            (asserts! (is-eq (get verification-hash part-data) verification-hash) ERR_VERIFICATION_FAILED)
            (asserts! (not (get verified part-data)) ERR_PART_ALREADY_VERIFIED)
            
            (map-set parts part-id (merge part-data { verified: true }))
            
            (map-set part-verifications part-id {
                verifier: tx-sender,
                verification-date: stacks-block-height,
                authenticity-score: authenticity-score,
                notes: notes
            })
            
            (ok true)
        )
    )
)

(define-public (transfer-part (part-id uint) (new-owner principal))
    (let
        (
            (ownership-info (map-get? part-ownership part-id))
            (part-info (map-get? parts part-id))
        )
        (asserts! (is-some part-info) ERR_PART_NOT_FOUND)
        (asserts! (is-some ownership-info) ERR_PART_NOT_FOUND)
        
        (let ((ownership-data (unwrap-panic ownership-info)))
            (asserts! (is-eq (get current-owner ownership-data) tx-sender) ERR_UNAUTHORIZED)
            
            (let ((updated-previous-owners (unwrap-panic (as-max-len? (append (get previous-owners ownership-data) tx-sender) u10))))
                (map-set part-ownership part-id {
                    current-owner: new-owner,
                    previous-owners: updated-previous-owners,
                    transfer-count: (+ (get transfer-count ownership-data) u1)
                })
            )
            
            (ok true)
        )
    )
)

(define-public (update-manufacturer-status (manufacturer principal) (verified bool))
    (let
        (
            (manufacturer-info (map-get? manufacturers manufacturer))
        )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (is-some manufacturer-info) ERR_INVALID_MANUFACTURER)
        
        (let ((manufacturer-data (unwrap-panic manufacturer-info)))
            (map-set manufacturers manufacturer (merge manufacturer-data { verified: verified }))
            (ok true)
        )
    )
)

(define-read-only (get-part-info (part-id uint))
    (map-get? parts part-id)
)

(define-read-only (get-part-ownership (part-id uint))
    (map-get? part-ownership part-id)
)

(define-read-only (get-manufacturer-info (manufacturer principal))
    (map-get? manufacturers manufacturer)
)

(define-read-only (get-part-verification (part-id uint))
    (map-get? part-verifications part-id)
)

(define-read-only (get-manufacturer-parts (manufacturer principal))
    (map-get? manufacturer-parts manufacturer)
)

(define-read-only (is-part-authentic (part-id uint) (verification-hash (buff 32)))
    (match (map-get? parts part-id)
        part-data (is-eq (get verification-hash part-data) verification-hash)
        false
    )
)

(define-read-only (get-part-history (part-id uint))
    (let
        (
            (part-info (map-get? parts part-id))
            (ownership-info (map-get? part-ownership part-id))
            (verification-info (map-get? part-verifications part-id))
        )
        {
            part: part-info,
            ownership: ownership-info,
            verification: verification-info
        }
    )
)

(define-read-only (get-next-part-id)
    (var-get next-part-id)
)

(define-read-only (is-manufacturer-verified (manufacturer principal))
    (match (map-get? manufacturers manufacturer)
        manufacturer-data (get verified manufacturer-data)
        false
    )
)

(define-public (create-warranty 
    (part-id uint)
    (warranty-type (string-ascii 50))
    (duration-blocks uint)
    (coverage-description (string-ascii 200))
    (terms-conditions (string-ascii 500)))
    (let
        (
            (warranty-id (var-get next-warranty-id))
            (part-info (map-get? parts part-id))
            (manufacturer-info (map-get? manufacturers tx-sender))
        )
        (asserts! (is-some part-info) ERR_PART_NOT_FOUND)
        (asserts! (is-some manufacturer-info) ERR_INVALID_MANUFACTURER)
        (asserts! (get verified (unwrap-panic manufacturer-info)) ERR_INVALID_MANUFACTURER)
        (asserts! (is-eq (get manufacturer (unwrap-panic part-info)) tx-sender) ERR_UNAUTHORIZED)
        (asserts! (> duration-blocks u0) ERR_INVALID_WARRANTY_DURATION)
        
        (let ((end-block (+ stacks-block-height duration-blocks)))
            (map-set part-warranties warranty-id {
                warranty-id: warranty-id,
                part-id: part-id,
                manufacturer: tx-sender,
                warranty-type: warranty-type,
                duration-blocks: duration-blocks,
                coverage-description: coverage-description,
                terms-conditions: terms-conditions,
                start-block: stacks-block-height,
                end-block: end-block,
                active: true,
                created-block: stacks-block-height
            })
            
            (map-set part-warranty-lookup part-id warranty-id)
            (var-set next-warranty-id (+ warranty-id u1))
            (ok warranty-id)
        )
    )
)

(define-public (submit-warranty-claim 
    (part-id uint)
    (claim-type (string-ascii 50))
    (description (string-ascii 300))
    (evidence-hash (buff 32)))
    (let
        (
            (claim-id (var-get next-claim-id))
            (warranty-id-opt (map-get? part-warranty-lookup part-id))
            (ownership-info (map-get? part-ownership part-id))
        )
        (asserts! (is-some warranty-id-opt) ERR_WARRANTY_NOT_FOUND)
        (asserts! (is-some ownership-info) ERR_PART_NOT_FOUND)
        (asserts! (is-eq (get current-owner (unwrap-panic ownership-info)) tx-sender) ERR_UNAUTHORIZED)
        
        (let 
            (
                (warranty-id (unwrap-panic warranty-id-opt))
                (warranty-info (map-get? part-warranties warranty-id))
            )
            (asserts! (is-some warranty-info) ERR_WARRANTY_NOT_FOUND)
            (let ((warranty-data (unwrap-panic warranty-info)))
                (asserts! (get active warranty-data) ERR_WARRANTY_EXPIRED)
                (asserts! (<= stacks-block-height (get end-block warranty-data)) ERR_WARRANTY_EXPIRED)
                
                (map-set warranty-claims claim-id {
                    claim-id: claim-id,
                    warranty-id: warranty-id,
                    part-id: part-id,
                    claimant: tx-sender,
                    claim-type: claim-type,
                    description: description,
                    evidence-hash: evidence-hash,
                    claim-date: stacks-block-height,
                    status: "pending",
                    resolution-notes: "",
                    approved-by: none,
                    processing-date: none,
                    settlement-amount: none
                })
                
                (let 
                    (
                        (manufacturer (get manufacturer warranty-data))
                        (current-claims (default-to (list) (map-get? manufacturer-warranty-claims manufacturer)))
                    )
                    (map-set manufacturer-warranty-claims manufacturer 
                        (unwrap-panic (as-max-len? (append current-claims claim-id) u500)))
                )
                
                (var-set next-claim-id (+ claim-id u1))
                (ok claim-id)
            )
        )
    )
)

(define-public (process-warranty-claim 
    (claim-id uint)
    (status (string-ascii 20))
    (resolution-notes (string-ascii 200))
    (settlement-amount (optional uint)))
    (let
        (
            (claim-info (map-get? warranty-claims claim-id))
        )
        (asserts! (is-some claim-info) ERR_CLAIM_NOT_FOUND)
        
        (let 
            (
                (claim-data (unwrap-panic claim-info))
                (warranty-info (map-get? part-warranties (get warranty-id claim-data)))
            )
            (asserts! (is-some warranty-info) ERR_WARRANTY_NOT_FOUND)
            (let ((warranty-data (unwrap-panic warranty-info)))
                (asserts! (is-eq (get manufacturer warranty-data) tx-sender) ERR_UNAUTHORIZED)
                (asserts! (is-eq (get status claim-data) "pending") ERR_CLAIM_ALREADY_PROCESSED)
                (asserts! (or (is-eq status "approved") (is-eq status "rejected")) ERR_INVALID_CLAIM_STATUS)
                
                (map-set warranty-claims claim-id (merge claim-data {
                    status: status,
                    resolution-notes: resolution-notes,
                    approved-by: (some tx-sender),
                    processing-date: (some stacks-block-height),
                    settlement-amount: settlement-amount
                }))
                
                (ok true)
            )
        )
    )
)

(define-public (deactivate-warranty (warranty-id uint))
    (let
        (
            (warranty-info (map-get? part-warranties warranty-id))
        )
        (asserts! (is-some warranty-info) ERR_WARRANTY_NOT_FOUND)
        
        (let ((warranty-data (unwrap-panic warranty-info)))
            (asserts! (is-eq (get manufacturer warranty-data) tx-sender) ERR_UNAUTHORIZED)
            (asserts! (get active warranty-data) ERR_WARRANTY_EXPIRED)
            
            (map-set part-warranties warranty-id (merge warranty-data { active: false }))
            (ok true)
        )
    )
)

(define-public (extend-warranty (warranty-id uint) (additional-blocks uint))
    (let
        (
            (warranty-info (map-get? part-warranties warranty-id))
        )
        (asserts! (is-some warranty-info) ERR_WARRANTY_NOT_FOUND)
        (asserts! (> additional-blocks u0) ERR_INVALID_WARRANTY_DURATION)
        
        (let ((warranty-data (unwrap-panic warranty-info)))
            (asserts! (is-eq (get manufacturer warranty-data) tx-sender) ERR_UNAUTHORIZED)
            (asserts! (get active warranty-data) ERR_WARRANTY_EXPIRED)
            
            (let ((new-end-block (+ (get end-block warranty-data) additional-blocks)))
                (map-set part-warranties warranty-id (merge warranty-data {
                    end-block: new-end-block,
                    duration-blocks: (+ (get duration-blocks warranty-data) additional-blocks)
                }))
                (ok new-end-block)
            )
        )
    )
)

(define-read-only (get-warranty-info (warranty-id uint))
    (map-get? part-warranties warranty-id)
)

(define-read-only (get-part-warranty (part-id uint))
    (match (map-get? part-warranty-lookup part-id)
        warranty-id (map-get? part-warranties warranty-id)
        none
    )
)

(define-read-only (get-claim-info (claim-id uint))
    (map-get? warranty-claims claim-id)
)

(define-read-only (get-manufacturer-claims (manufacturer principal))
    (map-get? manufacturer-warranty-claims manufacturer)
)

(define-read-only (is-warranty-active (warranty-id uint))
    (match (map-get? part-warranties warranty-id)
        warranty-data (and (get active warranty-data) (<= stacks-block-height (get end-block warranty-data)))
        false
    )
)

(define-read-only (get-warranty-status (part-id uint))
    (match (map-get? part-warranty-lookup part-id)
        warranty-id (match (map-get? part-warranties warranty-id)
            warranty-data {
                exists: true,
                active: (get active warranty-data),
                expired: (> stacks-block-height (get end-block warranty-data)),
                blocks-remaining: (if (<= stacks-block-height (get end-block warranty-data)) 
                    (- (get end-block warranty-data) stacks-block-height) 
                    u0)
            }
            { exists: false, active: false, expired: true, blocks-remaining: u0 }
        )
        { exists: false, active: false, expired: true, blocks-remaining: u0 }
    )
)

(define-read-only (get-next-warranty-id)
    (var-get next-warranty-id)
)

(define-read-only (get-next-claim-id)
    (var-get next-claim-id)
)

(define-public (submit-part-rating 
    (part-id uint)
    (overall-score uint)
    (durability-score uint)
    (performance-score uint)
    (value-score uint)
    (category (string-ascii 30))
    (review-text (string-ascii 300))
    (usage-duration uint))
    (let
        (
            (rating-id (var-get next-rating-id))
            (part-info (map-get? parts part-id))
            (ownership-info (map-get? part-ownership part-id))
            (existing-rating (map-get? user-part-ratings { user: tx-sender, part-id: part-id }))
        )
        (asserts! (is-some part-info) ERR_PART_NOT_FOUND)
        (asserts! (is-some ownership-info) ERR_PART_NOT_FOUND)
        (asserts! (is-none existing-rating) ERR_RATING_ALREADY_EXISTS)
        (asserts! (and (<= overall-score u10) (>= overall-score u1)) ERR_INVALID_RATING_SCORE)
        (asserts! (and (<= durability-score u10) (>= durability-score u1)) ERR_INVALID_RATING_SCORE)
        (asserts! (and (<= performance-score u10) (>= performance-score u1)) ERR_INVALID_RATING_SCORE)
        (asserts! (and (<= value-score u10) (>= value-score u1)) ERR_INVALID_RATING_SCORE)
        
        (let 
            (
                (ownership-data (unwrap-panic ownership-info))
                (part-data (unwrap-panic part-info))
                (is-current-owner (is-eq (get current-owner ownership-data) tx-sender))
                (is-previous-owner (is-some (index-of (get previous-owners ownership-data) tx-sender)))
                (can-rate (or is-current-owner is-previous-owner))
                (min-ownership-duration u1440)
            )
            (asserts! can-rate ERR_NOT_PART_OWNER)
            (asserts! (>= usage-duration min-ownership-duration) ERR_RATING_TOO_EARLY)
            
            (map-set part-ratings rating-id {
                rating-id: rating-id,
                part-id: part-id,
                rater: tx-sender,
                overall-score: overall-score,
                durability-score: durability-score,
                performance-score: performance-score,
                value-score: value-score,
                category: category,
                review-text: review-text,
                usage-duration: usage-duration,
                verified: (get verified part-data),
                rating-date: stacks-block-height,
                helpful-votes: u0,
                total-votes: u0
            })
            
            (map-set user-part-ratings { user: tx-sender, part-id: part-id } rating-id)
            (unwrap-panic (update-part-rating-aggregate part-id overall-score durability-score performance-score value-score (get verified part-data)))
            (unwrap-panic (update-manufacturer-rating-summary (get manufacturer part-data) overall-score))
            
            (var-set next-rating-id (+ rating-id u1))
            (ok rating-id)
        )
    )
)

(define-public (vote-rating-helpfulness (rating-id uint) (helpful bool))
    (let
        (
            (rating-info (map-get? part-ratings rating-id))
            (existing-vote (map-get? rating-helpfulness { rating-id: rating-id, voter: tx-sender }))
        )
        (asserts! (is-some rating-info) ERR_RATING_NOT_FOUND)
        (asserts! (is-none existing-vote) ERR_RATING_ALREADY_EXISTS)
        
        (let ((rating-data (unwrap-panic rating-info)))
            (map-set rating-helpfulness { rating-id: rating-id, voter: tx-sender } helpful)
            
            (let 
                (
                    (new-total-votes (+ (get total-votes rating-data) u1))
                    (new-helpful-votes (if helpful (+ (get helpful-votes rating-data) u1) (get helpful-votes rating-data)))
                )
                (map-set part-ratings rating-id (merge rating-data {
                    helpful-votes: new-helpful-votes,
                    total-votes: new-total-votes
                }))
                (ok true)
            )
        )
    )
)

(define-private (update-part-rating-aggregate 
    (part-id uint) 
    (overall-score uint) 
    (durability-score uint) 
    (performance-score uint) 
    (value-score uint)
    (verified bool))
    (let
        (
            (current-aggregate (default-to 
                {
                    total-ratings: u0,
                    average-overall: u0,
                    average-durability: u0,
                    average-performance: u0,
                    average-value: u0,
                    total-score-sum: u0,
                    last-updated: u0,
                    verified-ratings-count: u0
                }
                (map-get? part-rating-aggregates part-id)
            ))
        )
        (let 
            (
                (new-total-ratings (+ (get total-ratings current-aggregate) u1))
                (new-total-score-sum (+ (get total-score-sum current-aggregate) overall-score))
                (new-verified-count (if verified (+ (get verified-ratings-count current-aggregate) u1) (get verified-ratings-count current-aggregate)))
                (new-avg-overall (/ new-total-score-sum new-total-ratings))
                (new-avg-durability (/ (+ (* (get average-durability current-aggregate) (get total-ratings current-aggregate)) durability-score) new-total-ratings))
                (new-avg-performance (/ (+ (* (get average-performance current-aggregate) (get total-ratings current-aggregate)) performance-score) new-total-ratings))
                (new-avg-value (/ (+ (* (get average-value current-aggregate) (get total-ratings current-aggregate)) value-score) new-total-ratings))
            )
            (map-set part-rating-aggregates part-id {
                total-ratings: new-total-ratings,
                average-overall: new-avg-overall,
                average-durability: new-avg-durability,
                average-performance: new-avg-performance,
                average-value: new-avg-value,
                total-score-sum: new-total-score-sum,
                last-updated: stacks-block-height,
                verified-ratings-count: new-verified-count
            })
            (ok true)
        )
    )
)

(define-private (update-manufacturer-rating-summary (manufacturer principal) (rating uint))
    (let
        (
            (current-summary (default-to 
                {
                    total-parts-rated: u0,
                    total-ratings: u0,
                    average-rating: u0,
                    last-updated: u0,
                    reputation-score: u0
                }
                (map-get? manufacturer-rating-summary manufacturer)
            ))
        )
        (let 
            (
                (new-total-ratings (+ (get total-ratings current-summary) u1))
                (new-avg-rating (/ (+ (* (get average-rating current-summary) (get total-ratings current-summary)) rating) new-total-ratings))
                (new-reputation-score (calculate-reputation-score new-avg-rating new-total-ratings))
            )
            (map-set manufacturer-rating-summary manufacturer {
                total-parts-rated: (get total-parts-rated current-summary),
                total-ratings: new-total-ratings,
                average-rating: new-avg-rating,
                last-updated: stacks-block-height,
                reputation-score: new-reputation-score
            })
            (ok true)
        )
    )
)

(define-private (calculate-reputation-score (avg-rating uint) (total-ratings uint))
    (let 
        (
            (base-score (* avg-rating u10))
            (volume-bonus (if (>= total-ratings u50) u20 (if (>= total-ratings u20) u10 (if (>= total-ratings u10) u5 u0))))
        )
        (+ base-score volume-bonus)
    )
)

(define-public (flag-rating (rating-id uint) (reason (string-ascii 100)))
    (let
        (
            (rating-info (map-get? part-ratings rating-id))
        )
        (asserts! (is-some rating-info) ERR_RATING_NOT_FOUND)
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        
        (let ((rating-data (unwrap-panic rating-info)))
            (map-set part-ratings rating-id (merge rating-data { verified: false }))
            (ok true)
        )
    )
)

(define-read-only (get-part-rating (rating-id uint))
    (map-get? part-ratings rating-id)
)

(define-read-only (get-part-rating-aggregate (part-id uint))
    (map-get? part-rating-aggregates part-id)
)

(define-read-only (get-manufacturer-rating-summary (manufacturer principal))
    (map-get? manufacturer-rating-summary manufacturer)
)

(define-read-only (get-user-rating-for-part (user principal) (part-id uint))
    (match (map-get? user-part-ratings { user: user, part-id: part-id })
        rating-id (map-get? part-ratings rating-id)
        none
    )
)

(define-read-only (has-user-rated-part (user principal) (part-id uint))
    (is-some (map-get? user-part-ratings { user: user, part-id: part-id }))
)

(define-read-only (get-rating-helpfulness-vote (rating-id uint) (voter principal))
    (map-get? rating-helpfulness { rating-id: rating-id, voter: voter })
)

(define-read-only (calculate-part-quality-score (part-id uint))
    (match (map-get? part-rating-aggregates part-id)
        aggregate-data (let 
            (
                (base-score (get average-overall aggregate-data))
                (verified-bonus (if (>= (get verified-ratings-count aggregate-data) u3) u1 u0))
                (volume-bonus (if (>= (get total-ratings aggregate-data) u10) u1 u0))
            )
            (+ base-score verified-bonus volume-bonus)
        )
        u0
    )
)

(define-read-only (get-top-rated-manufacturer-parts (manufacturer principal) (min-rating uint))
    (default-to (list) (map-get? manufacturer-parts manufacturer))
)

(define-read-only (get-next-rating-id)
    (var-get next-rating-id)
)


