---
layout: default
title: CSL Application-Level Messaging
permalink: /protocols/csl-application-level/
group: protocols
---

# CSL Application-Level Messaging

In this document we explore messaging in Cardano SL. The goal of this
document is to get the reader acquainted with the way all the pieces,
such as Time-Warp, Network-Transport and Kademlia DHT, click together
making it possible to implement a full CSL node.

## Message Typeclass and Message Types

When you read the source code, you often encounter things
[like](https://github.com/input-output-hk/cardano-sl/blob/d564b3f5a7e03e086b62c88212870b5ea89f5e8b/src/Pos/Block/Network/Types.hs#L20-L29)

```
-- | 'GetHeaders' message (see protocol specification).
data MsgGetHeaders = MsgGetHeaders
    { -- not guaranteed to be in any particular order
      mghFrom :: !(NonEmpty HeaderHash)
    , mghTo   :: !(Maybe HeaderHash)
    } deriving (Generic, Show, Eq)

instance Message MsgGetHeaders where
    messageName _ = MessageName $ BC.pack "GetHeaders"
    formatMessage _ = "GetHeaders"
```

How to read this? Let's first examine `instance` part. This particular snippet
says that data structure defined by type `MsgGetHeaders` is used as a message
payload. Name of such a message is `"GetHeaders"`.

In this particular case, data structure has two fields — `mghFrom` and `mghTo`.
Prefixes like `mgh` are used because Haskell puts symbols for record fields in
the global namespace, so it's programmer's duty to avoid clashes.

It should be noted that sometimes you see messages that are parametrized with
`ssc` type variable. That is done for the code to be polymorphic with respect
to the way we carry out shared seed computation.
[Here](https://github.com/input-output-hk/cardano-sl/blob/d564b3f5a7e03e086b62c88212870b5ea89f5e8b/src/Pos/Block/Network/Types.hs#L42-L44)
is an example of a message that sends newest headers first, minding scc.

The way messages are serialized can be seen in
[Pos.Binary.Communication](https://github.com/input-output-hk/cardano-sl/blob/d564b3f5a7e03e086b62c88212870b5ea89f5e8b/src/Pos/Binary/Communication.hs)
module.

## Block Exchange Messages

This table explains [Pos.Block.Network.Types](https://github.com/input-output-hk/cardano-sl/blob/d564b3f5a7e03e086b62c88212870b5ea89f5e8b/src/Pos/Block/Network/Types.hs) module.

| Message Name | Payload | Commentaries |
|--------------|---------|--------------|
| GetHeaders | Oldest header hash we're interested in; Newest hash we're interested in, or blank | Expect newest header first |
| GetBlocks | Oldest header hash; Newest hash | As opposed to GetHeaders, both hashes have to be present |
| BlockHeaders | Non-empty collection of block headers, newest first | Polymorphic in scc |
| Block | A single block | Polymorphic in scc |

## Communication Messages

This table talks about data structures that are included in communication maintenance messages.
Modules covered are:

 + [Pos.Communication.Types.State](https://github.com/input-output-hk/cardano-sl/blob/d564b3f5a7e03e086b62c88212870b5ea89f5e8b/src/Pos/Communication/Types/State.hs)
 + [Pos.Communication.Types.Protocol](https://github.com/input-output-hk/cardano-sl/blob/d564b3f5a7e03e086b62c88212870b5ea89f5e8b/src/Pos/Communication/Types/Protocol.hs)
 + [Pos.Communication.Types.SysStart](https://github.com/input-output-hk/cardano-sl/blob/d564b3f5a7e03e086b62c88212870b5ea89f5e8b/src/Pos/Communication/Types/SysStart.hs)

| Data Type* or Message Name | Payload | Commentaries |
|----------------------------|---------|--------------|
| PeerState* | ProtocolVersion, or Nothing if we don't know version yet | Polymorphic in scc |
| VersionReq | Ø | Requests version information from a peer. To be answered with VersionResp |
| VersionResp | Int32 "magic" version bytes; Properly typed ProtocolVersion field | |
| SysStartRequest | Ø | Polls peer's timestamps. To be answered with SysStartResponse |
| SysStartResponse | Peer's timestamp | |

## Delegation Messages

This table describes delegation-related messages, found in
[Pos.Delegation.Types](https://github.com/input-output-hk/cardano-sl/blob/d564b3f5a7e03e086b62c88212870b5ea89f5e8b/src/Pos/Delegation/Types.hs)
module.

Delegation comes in two flavours — per-epoch delegation and delegation
with revokable long-lived certificates. Per-epoch delegation is called
“lightweight”, long-lived is called “heavyweight”.

| Message Name | Payload | Commentaries |
|--------------|---------|--------------|
| SendProxySK | ProxySKEpoch (lightweight) proxy secret key _or_ SendProxySKSimple (heavyweight) proxy secret key | |
| ConfirmProxySK | Signature of ProxySKEpoch with the proxy key itself | Used to confirm proxy signature delivery |
| CheckProxySKConfirmed | Ø | Checks if node is aware of PSK delivery. To be responded with CheckProxySKConfirmedRes |
| CheckProxySKConfirmedRes | Boolean | |
