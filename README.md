# Community Moderation DAO for Forums

A decentralized forum moderation system built on Stacks blockchain where token holders can participate in content moderation.

## 🌟 Features

- Create posts with content
- Upvote/downvote system
- Token-based moderation
- Transparent moderation actions
- User token management

## 🔧 Contract Functions

### Post Management
- `create-post`: Create a new forum post
- `upvote`: Upvote a post
- `downvote`: Downvote a post
- `moderate-post`: Moderate a post (requires tokens)

### Token Management
- `mint-tokens`: Mint moderation tokens
- `get-user-tokens`: Check token balance

### Read-Only Functions
- `get-post`: Get post details
- `get-user-vote`: Check user's vote on a post
- `get-moderator-action`: View moderation history

## 🚀 Usage

1. Deploy the contract using Clarinet
2. Mint tokens to participate in moderation
3. Create posts using `create-post`
4. Vote on posts using `upvote` or `downvote`
5. Moderate content with sufficient tokens using `moderate-post`

## ⚙️ Requirements

- Clarinet
- Stacks blockchain wallet
- Minimum 100 tokens for moderation actions

## 📝 Notes

The contract implements a simple but effective moderation system where token holders have the power to moderate content. All actions are recorded on-chain for transparency.
```
