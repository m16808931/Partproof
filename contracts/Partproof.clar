(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_PART_NOT_FOUND (err u101))
(define-constant ERR_PART_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_MANUFACTURER (err u103))
(define-constant ERR_PART_ALREADY_VERIFIED (err u104))
(define-constant ERR_VERIFICATION_FAILED (err u105))

(define-data-var next-part-id uint u1)

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