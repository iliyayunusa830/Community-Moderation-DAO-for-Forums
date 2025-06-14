(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-POST (err u101))
(define-constant ERR-ALREADY-VOTED (err u102))
(define-constant ERR-INSUFFICIENT-TOKENS (err u103))
(define-constant ERR-POST-NOT-FOUND (err u104))
(define-constant ERR-INVALID-AMOUNT (err u105))

(define-data-var min-tokens-to-moderate uint u100)
(define-data-var total-posts uint u0)

(define-map posts
    { post-id: uint }
    {
        author: principal,
        content: (string-ascii 280),
        upvotes: uint,
        downvotes: uint,
        status: (string-ascii 20),
        created-at: uint,
    }
)

(define-map user-votes
    {
        post-id: uint,
        voter: principal,
    }
    { vote-type: (string-ascii 10) }
)

(define-map moderator-actions
    {
        post-id: uint,
        moderator: principal,
    }
    {
        action: (string-ascii 20),
        timestamp: uint,
    }
)

(define-map user-tokens
    { user: principal }
    { balance: uint }
)

(define-public (create-post (content (string-ascii 280)))
    (let ((post-id (var-get total-posts)))
        (map-set posts { post-id: post-id } {
            author: tx-sender,
            content: content,
            upvotes: u0,
            downvotes: u0,
            status: "active",
            created-at: burn-block-height,
        })
        (var-set total-posts (+ post-id u1))
        (ok post-id)
    )
)

(define-public (upvote (post-id uint))
    (let (
            (post (unwrap! (map-get? posts { post-id: post-id }) ERR-POST-NOT-FOUND))
            (existing-vote (map-get? user-votes {
                post-id: post-id,
                voter: tx-sender,
            }))
        )
        (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
        (map-set posts { post-id: post-id }
            (merge post { upvotes: (+ (get upvotes post) u1) })
        )
        (map-set user-votes {
            post-id: post-id,
            voter: tx-sender,
        } { vote-type: "upvote" }
        )
        (ok true)
    )
)

(define-public (downvote (post-id uint))
    (let (
            (post (unwrap! (map-get? posts { post-id: post-id }) ERR-POST-NOT-FOUND))
            (existing-vote (map-get? user-votes {
                post-id: post-id,
                voter: tx-sender,
            }))
        )
        (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
        (map-set posts { post-id: post-id }
            (merge post { downvotes: (+ (get downvotes post) u1) })
        )
        (map-set user-votes {
            post-id: post-id,
            voter: tx-sender,
        } { vote-type: "downvote" }
        )
        (ok true)
    )
)

(define-public (moderate-post
        (post-id uint)
        (action (string-ascii 20))
    )
    (let (
            (post (unwrap! (map-get? posts { post-id: post-id }) ERR-POST-NOT-FOUND))
            (user-balance (default-to { balance: u0 }
                (map-get? user-tokens { user: tx-sender })
            ))
        )
        (asserts!
            (>= (get balance user-balance) (var-get min-tokens-to-moderate))
            ERR-INSUFFICIENT-TOKENS
        )
        (map-set posts { post-id: post-id } (merge post { status: action }))
        (map-set moderator-actions {
            post-id: post-id,
            moderator: tx-sender,
        } {
            action: action,
            timestamp: burn-block-height,
        })
        (ok true)
    )
)

(define-public (mint-tokens (amount uint))
    (let ((current-balance (default-to { balance: u0 } (map-get? user-tokens { user: tx-sender }))))
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (map-set user-tokens { user: tx-sender } { balance: (+ (get balance current-balance) amount) })
        (ok true)
    )
)

(define-read-only (get-post (post-id uint))
    (map-get? posts { post-id: post-id })
)

(define-read-only (get-user-vote
        (post-id uint)
        (user principal)
    )
    (map-get? user-votes {
        post-id: post-id,
        voter: user,
    })
)

(define-read-only (get-moderator-action
        (post-id uint)
        (moderator principal)
    )
    (map-get? moderator-actions {
        post-id: post-id,
        moderator: moderator,
    })
)

(define-read-only (get-user-tokens (user principal))
    (default-to { balance: u0 } (map-get? user-tokens { user: user }))
)
