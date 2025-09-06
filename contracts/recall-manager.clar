;; Part Recall Management System
;; Enables manufacturers to issue safety recalls and track resolution status

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_RECALL_NOT_FOUND (err u201))
(define-constant ERR_RECALL_ALREADY_EXISTS (err u202))
(define-constant ERR_INVALID_MANUFACTURER (err u203))
(define-constant ERR_RECALL_ALREADY_CLOSED (err u204))
(define-constant ERR_PART_NOT_AFFECTED (err u205))
(define-constant ERR_RESPONSE_ALREADY_SUBMITTED (err u206))
(define-constant ERR_INVALID_SEVERITY_LEVEL (err u207))
(define-constant ERR_RECALL_NOT_ACTIVE (err u208))

(define-data-var next-recall-id uint u1)

;; Store recall information
(define-map recalls
    uint
    {
        recall-id: uint,
        manufacturer: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        affected-parts: (list 100 uint),
        severity-level: uint, ;; 1-5 scale (1=low, 5=critical)
        recall-type: (string-ascii 50),
        remedy-description: (string-ascii 300),
        contact-info: (string-ascii 100),
        issue-date: uint,
        expected-resolution-date: uint,
        status: (string-ascii 20), ;; "active", "resolved", "closed"
        total-affected-count: uint,
        resolved-count: uint,
        regulatory-number: (string-ascii 50)
    }
)

;; Track which parts are affected by which recalls
(define-map part-recall-status
    { part-id: uint, recall-id: uint }
    {
        affected: bool,
        owner-notified: bool,
        resolved: bool,
        resolution-date: (optional uint),
        resolution-method: (optional (string-ascii 100))
    }
)

;; Store owner responses to recalls
(define-map recall-responses
    { recall-id: uint, responder: principal }
    {
        response-date: uint,
        action-taken: (string-ascii 100),
        repair-shop: (optional (string-ascii 100)),
        replacement-part-id: (optional uint),
        cost-covered: bool,
        satisfaction-score: uint
    }
)

;; Track manufacturer recall statistics
(define-map manufacturer-recall-stats
    principal
    {
        total-recalls: uint,
        active-recalls: uint,
        resolved-recalls: uint,
        total-parts-recalled: uint,
        average-resolution-time: uint,
        last-recall-date: uint
    }
)

;; Create a new safety recall
(define-public (issue-recall
    (title (string-ascii 100))
    (description (string-ascii 500))
    (affected-parts (list 100 uint))
    (severity-level uint)
    (recall-type (string-ascii 50))
    (remedy-description (string-ascii 300))
    (contact-info (string-ascii 100))
    (expected-resolution-blocks uint)
    (regulatory-number (string-ascii 50)))
    (let
        (
            (recall-id (var-get next-recall-id))
            (manufacturer-info (contract-call? .Partproof get-manufacturer-info tx-sender))
        )
        (asserts! (is-some manufacturer-info) ERR_INVALID_MANUFACTURER)
        (asserts! (and (<= severity-level u5) (>= severity-level u1)) ERR_INVALID_SEVERITY_LEVEL)
        
        (map-set recalls recall-id {
            recall-id: recall-id,
            manufacturer: tx-sender,
            title: title,
            description: description,
            affected-parts: affected-parts,
            severity-level: severity-level,
            recall-type: recall-type,
            remedy-description: remedy-description,
            contact-info: contact-info,
            issue-date: stacks-block-height,
            expected-resolution-date: (+ stacks-block-height expected-resolution-blocks),
            status: "active",
            total-affected-count: (len affected-parts),
            resolved-count: u0,
            regulatory-number: regulatory-number
        })
        
        ;; Mark all affected parts
        (unwrap-panic (mark-parts-as-affected recall-id affected-parts))
        (unwrap-panic (update-manufacturer-recall-stats tx-sender recall-id))
        
        (var-set next-recall-id (+ recall-id u1))
        (ok recall-id)
    )
)

;; Submit response to a recall as part owner
(define-public (respond-to-recall
    (recall-id uint)
    (action-taken (string-ascii 100))
    (repair-shop (optional (string-ascii 100)))
    (replacement-part-id (optional uint))
    (satisfaction-score uint))
    (let
        (
            (recall-info (map-get? recalls recall-id))
            (existing-response (map-get? recall-responses { recall-id: recall-id, responder: tx-sender }))
        )
        (asserts! (is-some recall-info) ERR_RECALL_NOT_FOUND)
        (asserts! (is-none existing-response) ERR_RESPONSE_ALREADY_SUBMITTED)
        (asserts! (is-eq (get status (unwrap-panic recall-info)) "active") ERR_RECALL_NOT_ACTIVE)
        (asserts! (and (<= satisfaction-score u10) (>= satisfaction-score u1)) ERR_INVALID_SEVERITY_LEVEL)
        
        (map-set recall-responses { recall-id: recall-id, responder: tx-sender } {
            response-date: stacks-block-height,
            action-taken: action-taken,
            repair-shop: repair-shop,
            replacement-part-id: replacement-part-id,
            cost-covered: (match repair-shop some-shop true false),
            satisfaction-score: satisfaction-score
        })
        
        (ok true)
    )
)

;; Mark a part's recall issue as resolved
(define-public (resolve-part-recall
    (part-id uint)
    (recall-id uint)
    (resolution-method (string-ascii 100)))
    (let
        (
            (recall-info (map-get? recalls recall-id))
            (part-status (map-get? part-recall-status { part-id: part-id, recall-id: recall-id }))
        )
        (asserts! (is-some recall-info) ERR_RECALL_NOT_FOUND)
        (asserts! (is-some part-status) ERR_PART_NOT_AFFECTED)
        
        (let 
            (
                (recall-data (unwrap-panic recall-info))
                (status-data (unwrap-panic part-status))
            )
            (asserts! (is-eq (get manufacturer recall-data) tx-sender) ERR_UNAUTHORIZED)
            (asserts! (not (get resolved status-data)) ERR_RECALL_ALREADY_CLOSED)
            
            (map-set part-recall-status { part-id: part-id, recall-id: recall-id }
                (merge status-data {
                    resolved: true,
                    resolution-date: (some stacks-block-height),
                    resolution-method: (some resolution-method)
                }))
            
            ;; Update recall resolved count
            (map-set recalls recall-id 
                (merge recall-data { resolved-count: (+ (get resolved-count recall-data) u1) }))
            
            (ok true)
        )
    )
)

;; Close a recall when all issues are resolved
(define-public (close-recall (recall-id uint))
    (let
        (
            (recall-info (map-get? recalls recall-id))
        )
        (asserts! (is-some recall-info) ERR_RECALL_NOT_FOUND)
        
        (let ((recall-data (unwrap-panic recall-info)))
            (asserts! (is-eq (get manufacturer recall-data) tx-sender) ERR_UNAUTHORIZED)
            (asserts! (>= (get resolved-count recall-data) (get total-affected-count recall-data)) ERR_RECALL_NOT_ACTIVE)
            
            (map-set recalls recall-id (merge recall-data { status: "closed" }))
            (ok true)
        )
    )
)

;; Private function to mark parts as affected by recall
(define-private (mark-parts-as-affected (recall-id uint) (part-list (list 100 uint)))
    (fold mark-single-part part-list (ok true))
)

(define-private (mark-single-part (part-id uint) (result (response bool uint)))
    (match result
        success (begin
            (map-set part-recall-status { part-id: part-id, recall-id: (var-get next-recall-id) } {
                affected: true,
                owner-notified: false,
                resolved: false,
                resolution-date: none,
                resolution-method: none
            })
            (ok true)
        )
        error result
    )
)

;; Update manufacturer recall statistics
(define-private (update-manufacturer-recall-stats (manufacturer principal) (recall-id uint))
    (let
        (
            (current-stats (default-to 
                {
                    total-recalls: u0,
                    active-recalls: u0,
                    resolved-recalls: u0,
                    total-parts-recalled: u0,
                    average-resolution-time: u0,
                    last-recall-date: u0
                }
                (map-get? manufacturer-recall-stats manufacturer)
            ))
        )
        (map-set manufacturer-recall-stats manufacturer
            (merge current-stats {
                total-recalls: (+ (get total-recalls current-stats) u1),
                active-recalls: (+ (get active-recalls current-stats) u1),
                last-recall-date: stacks-block-height
            }))
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-recall-info (recall-id uint))
    (map-get? recalls recall-id)
)

(define-read-only (get-part-recall-status (part-id uint) (recall-id uint))
    (map-get? part-recall-status { part-id: part-id, recall-id: recall-id })
)

(define-read-only (get-recall-response (recall-id uint) (responder principal))
    (map-get? recall-responses { recall-id: recall-id, responder: responder })
)

(define-read-only (get-manufacturer-recall-stats (manufacturer principal))
    (map-get? manufacturer-recall-stats manufacturer)
)

(define-read-only (is-part-recalled (part-id uint))
    (is-some (map-get? part-recall-status { part-id: part-id, recall-id: u1 }))
)

(define-read-only (get-active-recalls-by-manufacturer (manufacturer principal))
    (filter check-recall-active 
        (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)) ;; Simplified for demo
)

(define-private (check-recall-active (recall-id uint))
    (match (map-get? recalls recall-id)
        recall-data (and 
            (is-eq (get manufacturer recall-data) CONTRACT_OWNER)
            (is-eq (get status recall-data) "active"))
        false
    )
)

(define-read-only (get-recall-severity-stats)
    ;; Returns aggregate statistics about recall severity distribution
    {
        total-recalls: (var-get next-recall-id),
        critical-recalls: u0, ;; Simplified for demo
        high-recalls: u0,
        medium-recalls: u0,
        low-recalls: u0
    }
)

(define-read-only (get-next-recall-id)
    (var-get next-recall-id)
)
