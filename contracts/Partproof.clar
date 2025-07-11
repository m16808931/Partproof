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

(define-data-var next-part-id uint u1)
(define-data-var next-warranty-id uint u1)
(define-data-var next-claim-id uint u1)

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