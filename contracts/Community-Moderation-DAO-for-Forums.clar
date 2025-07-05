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

(define-constant ERR-INSUFFICIENT-REPUTATION (err u106))
(define-constant ERR-CANNOT-RATE-OWN-POST (err u107))
(define-constant ERR-ALREADY-RATED (err u108))

(define-data-var min-reputation-for-enhanced-moderation uint u50)

(define-map user-reputation
    { user: principal }
    {
        score: uint,
        total-posts: uint,
        successful-moderations: uint,
    }
)

(define-map post-ratings
    {
        post-id: uint,
        rater: principal,
    }
    {
        quality-score: uint,
        timestamp: uint,
    }
)

(define-map moderation-feedback
    {
        post-id: uint,
        moderator: principal,
        feedback-giver: principal,
    }
    {
        helpful: bool,
        timestamp: uint,
    }
)

(define-public (rate-post-quality
        (post-id uint)
        (quality-score uint)
    )
    (let (
            (post (unwrap! (map-get? posts { post-id: post-id }) ERR-POST-NOT-FOUND))
            (existing-rating (map-get? post-ratings {
                post-id: post-id,
                rater: tx-sender,
            }))
        )
        (asserts! (not (is-eq tx-sender (get author post)))
            ERR-CANNOT-RATE-OWN-POST
        )
        (asserts! (is-none existing-rating) ERR-ALREADY-RATED)
        (asserts! (and (>= quality-score u1) (<= quality-score u5))
            ERR-INVALID-AMOUNT
        )
        (map-set post-ratings {
            post-id: post-id,
            rater: tx-sender,
        } {
            quality-score: quality-score,
            timestamp: burn-block-height,
        })
        (let ((author-rep (default-to {
                score: u0,
                total-posts: u0,
                successful-moderations: u0,
            }
                (map-get? user-reputation { user: (get author post) })
            )))
            (map-set user-reputation { user: (get author post) } {
                score: (+ (get score author-rep) quality-score),
                total-posts: (get total-posts author-rep),
                successful-moderations: (get successful-moderations author-rep),
            })
        )
        (ok true)
    )
)

(define-public (rate-moderation-action
        (post-id uint)
        (moderator principal)
        (helpful bool)
    )
    (let (
            (moderation-action (unwrap!
                (map-get? moderator-actions {
                    post-id: post-id,
                    moderator: moderator,
                })
                ERR-POST-NOT-FOUND
            ))
            (existing-feedback (map-get? moderation-feedback {
                post-id: post-id,
                moderator: moderator,
                feedback-giver: tx-sender,
            }))
        )
        (asserts! (not (is-eq tx-sender moderator)) ERR-CANNOT-RATE-OWN-POST)
        (asserts! (is-none existing-feedback) ERR-ALREADY-RATED)
        (map-set moderation-feedback {
            post-id: post-id,
            moderator: moderator,
            feedback-giver: tx-sender,
        } {
            helpful: helpful,
            timestamp: burn-block-height,
        })
        (begin
            (if helpful
                (let ((mod-rep (default-to {
                        score: u0,
                        total-posts: u0,
                        successful-moderations: u0,
                    }
                        (map-get? user-reputation { user: moderator })
                    )))
                    (map-set user-reputation { user: moderator } {
                        score: (+ (get score mod-rep) u3),
                        total-posts: (get total-posts mod-rep),
                        successful-moderations: (+ (get successful-moderations mod-rep) u1),
                    })
                )
                true
            )
            (ok true)
        )
    )
)

(define-public (enhanced-moderate-post
        (post-id uint)
        (action (string-ascii 20))
    )
    (let (
            (post (unwrap! (map-get? posts { post-id: post-id }) ERR-POST-NOT-FOUND))
            (user-balance (default-to { balance: u0 }
                (map-get? user-tokens { user: tx-sender })
            ))
            (user-rep (default-to {
                score: u0,
                total-posts: u0,
                successful-moderations: u0,
            }
                (map-get? user-reputation { user: tx-sender })
            ))
        )
        (asserts!
            (or
                (>= (get balance user-balance) (var-get min-tokens-to-moderate))
                (>= (get score user-rep)
                    (var-get min-reputation-for-enhanced-moderation)
                )
            )
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

(define-read-only (get-user-reputation (user principal))
    (default-to {
        score: u0,
        total-posts: u0,
        successful-moderations: u0,
    }
        (map-get? user-reputation { user: user })
    )
)

(define-read-only (get-post-rating
        (post-id uint)
        (rater principal)
    )
    (map-get? post-ratings {
        post-id: post-id,
        rater: rater,
    })
)

(define-read-only (can-moderate-enhanced (user principal))
    (let (
            (user-balance (default-to { balance: u0 } (map-get? user-tokens { user: user })))
            (user-rep (default-to {
                score: u0,
                total-posts: u0,
                successful-moderations: u0,
            }
                (map-get? user-reputation { user: user })
            ))
        )
        (or
            (>= (get balance user-balance) (var-get min-tokens-to-moderate))
            (>= (get score user-rep)
                (var-get min-reputation-for-enhanced-moderation)
            )
        )
    )
)

(define-constant ERR-POST-EXPIRED (err u109))
(define-constant ERR-POST-ARCHIVED (err u110))
(define-constant ERR-INVALID-DURATION (err u111))

(define-data-var default-post-lifetime uint u1008)
(define-data-var archive-threshold uint u2016)
(define-data-var auto-moderation-threshold uint u144)

(define-map post-lifecycle
    { post-id: uint }
    {
        expires-at: uint,
        archived-at: (optional uint),
        auto-moderation-due: uint,
        lifecycle-status: (string-ascii 20),
    }
)

(define-map scheduled-actions
    { post-id: uint }
    {
        action-type: (string-ascii 20),
        scheduled-for: uint,
        executed: bool,
    }
)

(define-public (create-post-with-lifecycle
        (content (string-ascii 280))
        (custom-lifetime (optional uint))
    )
    (let (
            (post-id (var-get total-posts))
            (lifetime (default-to (var-get default-post-lifetime) custom-lifetime))
            (expires-at (+ burn-block-height lifetime))
        )
        (asserts! (> lifetime u0) ERR-INVALID-DURATION)
        (map-set posts { post-id: post-id } {
            author: tx-sender,
            content: content,
            upvotes: u0,
            downvotes: u0,
            status: "active",
            created-at: burn-block-height,
        })
        (map-set post-lifecycle { post-id: post-id } {
            expires-at: expires-at,
            archived-at: none,
            auto-moderation-due: (+ burn-block-height (var-get auto-moderation-threshold)),
            lifecycle-status: "active",
        })
        (var-set total-posts (+ post-id u1))
        (ok post-id)
    )
)

(define-public (extend-post-lifetime
        (post-id uint)
        (additional-blocks uint)
    )
    (let (
            (post (unwrap! (map-get? posts { post-id: post-id }) ERR-POST-NOT-FOUND))
            (lifecycle (unwrap! (map-get? post-lifecycle { post-id: post-id })
                ERR-POST-NOT-FOUND
            ))
            (user-balance (default-to { balance: u0 }
                (map-get? user-tokens { user: tx-sender })
            ))
        )
        (asserts! (is-eq tx-sender (get author post)) ERR-NOT-AUTHORIZED)
        (asserts! (>= (get balance user-balance) u10) ERR-INSUFFICIENT-TOKENS)
        (asserts! (> additional-blocks u0) ERR-INVALID-DURATION)
        (map-set post-lifecycle { post-id: post-id }
            (merge lifecycle { expires-at: (+ (get expires-at lifecycle) additional-blocks) })
        )
        (let ((current-balance (get balance user-balance)))
            (map-set user-tokens { user: tx-sender } { balance: (- current-balance u10) })
        )
        (ok true)
    )
)

(define-public (archive-expired-post (post-id uint))
    (let (
            (post (unwrap! (map-get? posts { post-id: post-id }) ERR-POST-NOT-FOUND))
            (lifecycle (unwrap! (map-get? post-lifecycle { post-id: post-id })
                ERR-POST-NOT-FOUND
            ))
        )
        (asserts! (<= (get expires-at lifecycle) burn-block-height)
            ERR-POST-NOT-FOUND
        )
        (asserts! (is-none (get archived-at lifecycle)) ERR-POST-ARCHIVED)
        (map-set posts { post-id: post-id } (merge post { status: "archived" }))
        (map-set post-lifecycle { post-id: post-id }
            (merge lifecycle {
                archived-at: (some burn-block-height),
                lifecycle-status: "archived",
            })
        )
        (ok true)
    )
)

(define-public (schedule-post-action
        (post-id uint)
        (action-type (string-ascii 20))
        (blocks-from-now uint)
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
        (asserts! (> blocks-from-now u0) ERR-INVALID-DURATION)
        (map-set scheduled-actions { post-id: post-id } {
            action-type: action-type,
            scheduled-for: (+ burn-block-height blocks-from-now),
            executed: false,
        })
        (ok true)
    )
)

(define-public (execute-scheduled-action (post-id uint))
    (let (
            (post (unwrap! (map-get? posts { post-id: post-id }) ERR-POST-NOT-FOUND))
            (scheduled (unwrap! (map-get? scheduled-actions { post-id: post-id })
                ERR-POST-NOT-FOUND
            ))
        )
        (asserts! (<= (get scheduled-for scheduled) burn-block-height)
            ERR-POST-NOT-FOUND
        )
        (asserts! (not (get executed scheduled)) ERR-ALREADY-VOTED)
        (map-set posts { post-id: post-id }
            (merge post { status: (get action-type scheduled) })
        )
        (map-set scheduled-actions { post-id: post-id }
            (merge scheduled { executed: true })
        )
        (map-set moderator-actions {
            post-id: post-id,
            moderator: tx-sender,
        } {
            action: (get action-type scheduled),
            timestamp: burn-block-height,
        })
        (ok true)
    )
)

(define-read-only (get-post-lifecycle (post-id uint))
    (map-get? post-lifecycle { post-id: post-id })
)

(define-read-only (get-scheduled-action (post-id uint))
    (map-get? scheduled-actions { post-id: post-id })
)

(define-read-only (is-post-expired (post-id uint))
    (match (map-get? post-lifecycle { post-id: post-id })
        lifecycle (>= burn-block-height (get expires-at lifecycle))
        false
    )
)

(define-read-only (get-posts-due-for-review)
    (let ((current-block burn-block-height))
        (ok current-block)
    )
)
