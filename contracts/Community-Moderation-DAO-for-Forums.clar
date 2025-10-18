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
        (unwrap-panic (update-engagement-on-vote post-id))
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
        (unwrap-panic (update-engagement-on-vote post-id))
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
(define-constant ERR-CONTENT-HASH-MISMATCH (err u112))
(define-constant ERR-VERIFICATION-ALREADY-EXISTS (err u113))

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

(define-map content-integrity
    { post-id: uint }
    {
        content-hash: (buff 32),
        verifier: principal,
        verified-at: uint,
        integrity-status: (string-ascii 20),
    }
)

(define-map verification-challenges
    {
        post-id: uint,
        challenger: principal,
    }
    {
        challenge-hash: (buff 32),
        challenge-timestamp: uint,
        resolution-status: (string-ascii 20),
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

(define-public (verify-content-integrity
        (post-id uint)
        (content-hash (buff 32))
    )
    (let (
            (post (unwrap! (map-get? posts { post-id: post-id }) ERR-POST-NOT-FOUND))
            (existing-verification (map-get? content-integrity { post-id: post-id }))
        )
        (asserts! (is-none existing-verification) ERR-VERIFICATION-ALREADY-EXISTS)
        (map-set content-integrity { post-id: post-id } {
            content-hash: content-hash,
            verifier: tx-sender,
            verified-at: burn-block-height,
            integrity-status: "verified",
        })
        (ok true)
    )
)

(define-public (challenge-content-integrity
        (post-id uint)
        (challenge-hash (buff 32))
    )
    (let (
            (post (unwrap! (map-get? posts { post-id: post-id }) ERR-POST-NOT-FOUND))
            (integrity-record (unwrap! (map-get? content-integrity { post-id: post-id })
                ERR-POST-NOT-FOUND
            ))
            (existing-challenge (map-get? verification-challenges {
                post-id: post-id,
                challenger: tx-sender,
            }))
        )
        (asserts! (is-none existing-challenge) ERR-ALREADY-VOTED)
        (asserts!
            (not (is-eq (get content-hash integrity-record) challenge-hash))
            ERR-CONTENT-HASH-MISMATCH
        )
        (map-set verification-challenges {
            post-id: post-id,
            challenger: tx-sender,
        } {
            challenge-hash: challenge-hash,
            challenge-timestamp: burn-block-height,
            resolution-status: "pending",
        })
        (map-set content-integrity { post-id: post-id }
            (merge integrity-record { integrity-status: "challenged" })
        )
        (ok true)
    )
)

(define-public (resolve-integrity-challenge
        (post-id uint)
        (challenger principal)
        (resolution (string-ascii 20))
    )
    (let (
            (post (unwrap! (map-get? posts { post-id: post-id }) ERR-POST-NOT-FOUND))
            (challenge (unwrap!
                (map-get? verification-challenges {
                    post-id: post-id,
                    challenger: challenger,
                })
                ERR-POST-NOT-FOUND
            ))
            (integrity-record (unwrap! (map-get? content-integrity { post-id: post-id })
                ERR-POST-NOT-FOUND
            ))
            (user-balance (default-to { balance: u0 }
                (map-get? user-tokens { user: tx-sender })
            ))
        )
        (asserts!
            (>= (get balance user-balance) (var-get min-tokens-to-moderate))
            ERR-INSUFFICIENT-TOKENS
        )
        (map-set verification-challenges {
            post-id: post-id,
            challenger: challenger,
        }
            (merge challenge { resolution-status: resolution })
        )
        (map-set content-integrity { post-id: post-id }
            (merge integrity-record { integrity-status: (if (is-eq resolution "upheld")
                "verified"
                "disputed"
            ) }
            ))
        (ok true)
    )
)

(define-read-only (get-content-integrity (post-id uint))
    (map-get? content-integrity { post-id: post-id })
)

(define-read-only (get-verification-challenge
        (post-id uint)
        (challenger principal)
    )
    (map-get? verification-challenges {
        post-id: post-id,
        challenger: challenger,
    })
)

(define-read-only (is-content-verified (post-id uint))
    (match (map-get? content-integrity { post-id: post-id })
        integrity-record (is-eq (get integrity-status integrity-record) "verified")
        false
    )
)

(define-constant ERR-SCORE-CALCULATION-FAILED (err u114))
(define-constant ERR-TRENDING-WINDOW-INVALID (err u115))

(define-data-var engagement-decay-factor uint u95)
(define-data-var trending-threshold uint u50)
(define-data-var score-precision-multiplier uint u100)

(define-map post-engagement-scores
    { post-id: uint }
    {
        current-score: uint,
        peak-score: uint,
        last-updated: uint,
        trending-status: bool,
        interaction-count: uint,
        score-velocity: uint,
    }
)

(define-map engagement-metrics
    { post-id: uint }
    {
        total-interactions: uint,
        unique-interactors: uint,
        quality-weighted-score: uint,
        freshness-boost: uint,
        controversy-score: uint,
    }
)

(define-map score-snapshots
    {
        post-id: uint,
        snapshot-height: uint,
    }
    {
        score-at-snapshot: uint,
        interactions-at-snapshot: uint,
    }
)

(define-public (calculate-engagement-score (post-id uint))
    (let (
            (post (unwrap! (map-get? posts { post-id: post-id }) ERR-POST-NOT-FOUND))
            (current-height burn-block-height)
            (upvotes (get upvotes post))
            (downvotes (get downvotes post))
            (post-age (- current-height (get created-at post)))
            (vote-ratio (if (> (+ upvotes downvotes) u0)
                (/ (* upvotes (var-get score-precision-multiplier))
                    (+ upvotes downvotes)
                )
                u50
            ))
            (freshness-factor (if (< post-age u144)
                (- u100 (/ (* post-age u70) u144))
                u30
            ))
            (interaction-score (+ upvotes downvotes))
            (quality-bonus (if (> upvotes (* downvotes u2))
                u20
                u0
            ))
            (controversy-factor (if (and
                    (> upvotes u0)
                    (> downvotes u0)
                    (< vote-ratio u80)
                    (> vote-ratio u20)
                )
                u15
                u0
            ))
            (base-score (+ (* vote-ratio u3) (* freshness-factor u2) (* interaction-score u5)
                quality-bonus controversy-factor
            ))
            (time-decayed-score (/ (* base-score (var-get engagement-decay-factor)) u100))
            (existing-score (default-to {
                current-score: u0,
                peak-score: u0,
                last-updated: u0,
                trending-status: false,
                interaction-count: u0,
                score-velocity: u0,
            }
                (map-get? post-engagement-scores { post-id: post-id })
            ))
            (velocity (if (> current-height (get last-updated existing-score))
                (if (> time-decayed-score (get current-score existing-score))
                    (- time-decayed-score (get current-score existing-score))
                    u0
                )
                u0
            ))
            (is-trending (>= velocity (var-get trending-threshold)))
        )
        (map-set post-engagement-scores { post-id: post-id } {
            current-score: time-decayed-score,
            peak-score: (if (> time-decayed-score (get peak-score existing-score))
                time-decayed-score
                (get peak-score existing-score)
            ),
            last-updated: current-height,
            trending-status: is-trending,
            interaction-count: (+ upvotes downvotes),
            score-velocity: velocity,
        })
        (map-set engagement-metrics { post-id: post-id } {
            total-interactions: (+ upvotes downvotes),
            unique-interactors: (+ upvotes downvotes),
            quality-weighted-score: (/ (* vote-ratio base-score) u100),
            freshness-boost: freshness-factor,
            controversy-score: controversy-factor,
        })
        (ok time-decayed-score)
    )
)

(define-public (batch-update-engagement-scores (post-ids (list 10 uint)))
    (fold update-single-engagement-score post-ids (ok u0))
)

(define-private (update-single-engagement-score
        (post-id uint)
        (previous-result (response uint uint))
    )
    (match previous-result
        success-value (calculate-engagement-score post-id)
        error-value (err error-value)
    )
)

(define-public (create-score-snapshot
        (post-id uint)
        (snapshot-height uint)
    )
    (let (
            (engagement-score (unwrap! (map-get? post-engagement-scores { post-id: post-id })
                ERR-POST-NOT-FOUND
            ))
            (metrics (unwrap! (map-get? engagement-metrics { post-id: post-id })
                ERR-POST-NOT-FOUND
            ))
        )
        (map-set score-snapshots {
            post-id: post-id,
            snapshot-height: snapshot-height,
        } {
            score-at-snapshot: (get current-score engagement-score),
            interactions-at-snapshot: (get total-interactions metrics),
        })
        (ok true)
    )
)

(define-public (boost-trending-post (post-id uint))
    (let (
            (engagement-score (unwrap! (map-get? post-engagement-scores { post-id: post-id })
                ERR-POST-NOT-FOUND
            ))
            (user-balance (default-to { balance: u0 }
                (map-get? user-tokens { user: tx-sender })
            ))
        )
        (asserts! (get trending-status engagement-score) ERR-POST-NOT-FOUND)
        (asserts! (>= (get balance user-balance) u5) ERR-INSUFFICIENT-TOKENS)
        (map-set post-engagement-scores { post-id: post-id }
            (merge engagement-score {
                current-score: (+ (get current-score engagement-score) u25),
                score-velocity: (+ (get score-velocity engagement-score) u10),
            })
        )
        (let ((current-balance (get balance user-balance)))
            (map-set user-tokens { user: tx-sender } { balance: (- current-balance u5) })
        )
        (ok true)
    )
)

(define-public (update-engagement-on-vote (post-id uint))
    (calculate-engagement-score post-id)
)

(define-read-only (get-engagement-score (post-id uint))
    (map-get? post-engagement-scores { post-id: post-id })
)

(define-read-only (get-engagement-metrics (post-id uint))
    (map-get? engagement-metrics { post-id: post-id })
)

(define-read-only (get-score-snapshot
        (post-id uint)
        (snapshot-height uint)
    )
    (map-get? score-snapshots {
        post-id: post-id,
        snapshot-height: snapshot-height,
    })
)

(define-read-only (is-post-trending (post-id uint))
    (match (map-get? post-engagement-scores { post-id: post-id })
        engagement-data (get trending-status engagement-data)
        false
    )
)

(define-read-only (get-trending-posts)
    (let ((current-height burn-block-height))
        (ok current-height)
    )
)

(define-read-only (compare-engagement-scores
        (post-id-a uint)
        (post-id-b uint)
    )
    (let (
            (score-a (default-to {
                current-score: u0,
                peak-score: u0,
                last-updated: u0,
                trending-status: false,
                interaction-count: u0,
                score-velocity: u0,
            }
                (map-get? post-engagement-scores { post-id: post-id-a })
            ))
            (score-b (default-to {
                current-score: u0,
                peak-score: u0,
                last-updated: u0,
                trending-status: false,
                interaction-count: u0,
                score-velocity: u0,
            }
                (map-get? post-engagement-scores { post-id: post-id-b })
            ))
        )
        (if (> (get current-score score-a) (get current-score score-b))
            post-id-a
            post-id-b
        )
    )
)

(define-read-only (get-engagement-leaderboard-position (post-id uint))
    (match (map-get? post-engagement-scores { post-id: post-id })
        engagement-data (get current-score engagement-data)
        u0
    )
)

(define-constant ERR-APPEAL-NOT-FOUND (err u116))
(define-constant ERR-APPEAL-ALREADY-EXISTS (err u117))
(define-constant ERR-APPEAL-CLOSED (err u118))
(define-constant ERR-ALREADY-VOTED-ON-APPEAL (err u119))
(define-constant ERR-APPEAL-NOT-READY (err u120))
(define-constant ERR-NO-MODERATION-TO-APPEAL (err u121))

(define-data-var appeal-id-nonce uint u0)
(define-data-var min-appeal-stake uint u50)
(define-data-var appeal-voting-period uint u144)
(define-data-var appeal-quorum-threshold uint u100)

(define-map appeals
    { appeal-id: uint }
    {
        post-id: uint,
        moderator: principal,
        appellant: principal,
        reason: (string-ascii 280),
        stake-amount: uint,
        votes-for-overturn: uint,
        votes-against-overturn: uint,
        status: (string-ascii 20),
        created-at: uint,
        resolved-at: (optional uint),
    }
)

(define-map appeal-votes
    {
        appeal-id: uint,
        voter: principal,
    }
    {
        vote-position: bool,
        token-weight: uint,
        voted-at: uint,
    }
)

(define-map moderation-appeals-map
    {
        post-id: uint,
        moderator: principal,
    }
    { appeal-id: uint }
)

(define-map user-appeal-stats
    { user: principal }
    {
        appeals-filed: uint,
        appeals-won: uint,
        appeals-lost: uint,
        total-stake-returned: uint,
    }
)

(define-map appeal-outcomes
    { appeal-id: uint }
    {
        outcome: (string-ascii 20),
        final-vote-count: uint,
        participation-rate: uint,
        stake-returned: bool,
    }
)

(define-public (create-appeal
        (post-id uint)
        (moderator principal)
        (reason (string-ascii 280))
    )
    (let (
            (moderation-action (unwrap!
                (map-get? moderator-actions {
                    post-id: post-id,
                    moderator: moderator,
                })
                ERR-NO-MODERATION-TO-APPEAL
            ))
            (existing-appeal (map-get? moderation-appeals-map {
                post-id: post-id,
                moderator: moderator,
            }))
            (user-balance (default-to { balance: u0 }
                (map-get? user-tokens { user: tx-sender })
            ))
            (appeal-id (var-get appeal-id-nonce))
            (stake-amount (var-get min-appeal-stake))
        )
        (asserts! (is-none existing-appeal) ERR-APPEAL-ALREADY-EXISTS)
        (asserts! (>= (get balance user-balance) stake-amount)
            ERR-INSUFFICIENT-TOKENS
        )
        (map-set appeals { appeal-id: appeal-id } {
            post-id: post-id,
            moderator: moderator,
            appellant: tx-sender,
            reason: reason,
            stake-amount: stake-amount,
            votes-for-overturn: u0,
            votes-against-overturn: u0,
            status: "open",
            created-at: burn-block-height,
            resolved-at: none,
        })
        (map-set moderation-appeals-map {
            post-id: post-id,
            moderator: moderator,
        } { appeal-id: appeal-id }
        )
        (map-set user-tokens { user: tx-sender } { balance: (- (get balance user-balance) stake-amount) })
        (let ((user-stats (default-to {
                appeals-filed: u0,
                appeals-won: u0,
                appeals-lost: u0,
                total-stake-returned: u0,
            }
                (map-get? user-appeal-stats { user: tx-sender })
            )))
            (map-set user-appeal-stats { user: tx-sender } {
                appeals-filed: (+ (get appeals-filed user-stats) u1),
                appeals-won: (get appeals-won user-stats),
                appeals-lost: (get appeals-lost user-stats),
                total-stake-returned: (get total-stake-returned user-stats),
            })
        )
        (var-set appeal-id-nonce (+ appeal-id u1))
        (ok appeal-id)
    )
)

(define-public (vote-on-appeal
        (appeal-id uint)
        (vote-for-overturn bool)
    )
    (let (
            (appeal (unwrap! (map-get? appeals { appeal-id: appeal-id })
                ERR-APPEAL-NOT-FOUND
            ))
            (existing-vote (map-get? appeal-votes {
                appeal-id: appeal-id,
                voter: tx-sender,
            }))
            (user-balance (default-to { balance: u0 }
                (map-get? user-tokens { user: tx-sender })
            ))
            (token-weight (get balance user-balance))
            (current-height burn-block-height)
            (appeal-deadline (+ (get created-at appeal) (var-get appeal-voting-period)))
        )
        (asserts! (is-eq (get status appeal) "open") ERR-APPEAL-CLOSED)
        (asserts! (< current-height appeal-deadline) ERR-APPEAL-CLOSED)
        (asserts! (is-none existing-vote) ERR-ALREADY-VOTED-ON-APPEAL)
        (asserts! (> token-weight u0) ERR-INSUFFICIENT-TOKENS)
        (map-set appeal-votes {
            appeal-id: appeal-id,
            voter: tx-sender,
        } {
            vote-position: vote-for-overturn,
            token-weight: token-weight,
            voted-at: current-height,
        })
        (map-set appeals { appeal-id: appeal-id }
            (merge appeal {
                votes-for-overturn: (if vote-for-overturn
                    (+ (get votes-for-overturn appeal) token-weight)
                    (get votes-for-overturn appeal)
                ),
                votes-against-overturn: (if vote-for-overturn
                    (get votes-against-overturn appeal)
                    (+ (get votes-against-overturn appeal) token-weight)
                ),
            })
        )
        (ok true)
    )
)

(define-public (resolve-appeal (appeal-id uint))
    (let (
            (appeal (unwrap! (map-get? appeals { appeal-id: appeal-id })
                ERR-APPEAL-NOT-FOUND
            ))
            (current-height burn-block-height)
            (appeal-deadline (+ (get created-at appeal) (var-get appeal-voting-period)))
            (total-votes (+ (get votes-for-overturn appeal)
                (get votes-against-overturn appeal)
            ))
            (overturn-wins (> (get votes-for-overturn appeal)
                (get votes-against-overturn appeal)
            ))
            (quorum-met (>= total-votes (var-get appeal-quorum-threshold)))
            (appellant (get appellant appeal))
            (stake (get stake-amount appeal))
        )
        (asserts! (is-eq (get status appeal) "open") ERR-APPEAL-CLOSED)
        (asserts! (>= current-height appeal-deadline) ERR-APPEAL-NOT-READY)
        (let (
                (final-status (if (and quorum-met overturn-wins)
                    "upheld"
                    "rejected"
                ))
                (return-stake (and quorum-met overturn-wins))
            )
            (map-set appeals { appeal-id: appeal-id }
                (merge appeal {
                    status: final-status,
                    resolved-at: (some current-height),
                })
            )
            (if return-stake
                (begin
                    (let ((appellant-balance (default-to { balance: u0 }
                            (map-get? user-tokens { user: appellant })
                        )))
                        (map-set user-tokens { user: appellant } { balance: (+ (get balance appellant-balance) (+ stake u25)) })
                    )
                    (let ((user-stats (default-to {
                            appeals-filed: u0,
                            appeals-won: u0,
                            appeals-lost: u0,
                            total-stake-returned: u0,
                        }
                            (map-get? user-appeal-stats { user: appellant })
                        )))
                        (map-set user-appeal-stats { user: appellant } {
                            appeals-filed: (get appeals-filed user-stats),
                            appeals-won: (+ (get appeals-won user-stats) u1),
                            appeals-lost: (get appeals-lost user-stats),
                            total-stake-returned: (+ (get total-stake-returned user-stats) stake),
                        })
                    )
                )
                (let ((user-stats (default-to {
                        appeals-filed: u0,
                        appeals-won: u0,
                        appeals-lost: u0,
                        total-stake-returned: u0,
                    }
                        (map-get? user-appeal-stats { user: appellant })
                    )))
                    (map-set user-appeal-stats { user: appellant } {
                        appeals-filed: (get appeals-filed user-stats),
                        appeals-won: (get appeals-won user-stats),
                        appeals-lost: (+ (get appeals-lost user-stats) u1),
                        total-stake-returned: (get total-stake-returned user-stats),
                    })
                )
            )
            (map-set appeal-outcomes { appeal-id: appeal-id } {
                outcome: final-status,
                final-vote-count: total-votes,
                participation-rate: (if (> total-votes u0)
                    (let ((threshold (var-get appeal-quorum-threshold)))
                        (/ (* total-votes u100)
                            (if (> total-votes threshold)
                                total-votes
                                threshold
                            ))
                    )
                    u0
                ),
                stake-returned: return-stake,
            })
            (if (and quorum-met overturn-wins)
                (let ((post (unwrap! (map-get? posts { post-id: (get post-id appeal) })
                        ERR-POST-NOT-FOUND
                    )))
                    (map-set posts { post-id: (get post-id appeal) }
                        (merge post { status: "active" })
                    )
                )
                true
            )
            (ok return-stake)
        )
    )
)

(define-public (cancel-appeal (appeal-id uint))
    (let (
            (appeal (unwrap! (map-get? appeals { appeal-id: appeal-id })
                ERR-APPEAL-NOT-FOUND
            ))
            (appellant (get appellant appeal))
            (stake (get stake-amount appeal))
        )
        (asserts! (is-eq tx-sender appellant) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status appeal) "open") ERR-APPEAL-CLOSED)
        (let ((total-votes (+ (get votes-for-overturn appeal)
                (get votes-against-overturn appeal)
            )))
            (asserts! (is-eq total-votes u0) ERR-ALREADY-VOTED)
            (map-set appeals { appeal-id: appeal-id }
                (merge appeal {
                    status: "cancelled",
                    resolved-at: (some burn-block-height),
                })
            )
            (let ((appellant-balance (default-to { balance: u0 }
                    (map-get? user-tokens { user: appellant })
                )))
                (map-set user-tokens { user: appellant } { balance: (+ (get balance appellant-balance) stake) })
            )
            (ok true)
        )
    )
)

(define-read-only (get-appeal (appeal-id uint))
    (map-get? appeals { appeal-id: appeal-id })
)

(define-read-only (get-appeal-vote
        (appeal-id uint)
        (voter principal)
    )
    (map-get? appeal-votes {
        appeal-id: appeal-id,
        voter: voter,
    })
)

(define-read-only (get-appeal-for-moderation
        (post-id uint)
        (moderator principal)
    )
    (map-get? moderation-appeals-map {
        post-id: post-id,
        moderator: moderator,
    })
)

(define-read-only (get-user-appeal-stats (user principal))
    (default-to {
        appeals-filed: u0,
        appeals-won: u0,
        appeals-lost: u0,
        total-stake-returned: u0,
    }
        (map-get? user-appeal-stats { user: user })
    )
)

(define-read-only (get-appeal-outcome (appeal-id uint))
    (map-get? appeal-outcomes { appeal-id: appeal-id })
)

(define-read-only (is-appeal-active (appeal-id uint))
    (match (map-get? appeals { appeal-id: appeal-id })
        appeal-data (and
            (is-eq (get status appeal-data) "open")
            (< burn-block-height
                (+ (get created-at appeal-data) (var-get appeal-voting-period))
            )
        )
        false
    )
)

(define-read-only (get-appeal-voting-power (user principal))
    (get balance
        (default-to { balance: u0 } (map-get? user-tokens { user: user }))
    )
)

(define-read-only (calculate-appeal-result (appeal-id uint))
    (match (map-get? appeals { appeal-id: appeal-id })
        appeal-data (let (
                (total-votes (+ (get votes-for-overturn appeal-data)
                    (get votes-against-overturn appeal-data)
                ))
                (overturn-percentage (if (> total-votes u0)
                    (/ (* (get votes-for-overturn appeal-data) u100) total-votes)
                    u0
                ))
            )
            (ok {
                total-votes: total-votes,
                overturn-percentage: overturn-percentage,
                quorum-met: (>= total-votes (var-get appeal-quorum-threshold)),
            })
        )
        ERR-APPEAL-NOT-FOUND
    )
)
