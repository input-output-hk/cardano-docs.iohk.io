---
layout: default
title: Update Mechanism
permalink: /cardano/update-mechanism/
group: cardano
visible: true
---

[//]: # (Reviewed at ce9174612676717d935c8f0237019b9c8449c718)

# Research Overview

In the Update Mechanism research, we have managed to propose an update
system that is capable of producing painless and almost seamless
software updates as well as providing stakeholders with an option to
vote for hard forks (backwards-incompatible protocol updates) without
the necessity to introduce any non-protocol-level tools.

We propose to use stake for voting for soft and hard forks and show
that it is possible to migrate value that exists on the unmaintained
blockchain to the new blockchain using a modified proof of burn scheme.

## Update System Model

For CSL, we decided to add some support for protocol updates at
the protocol layer itself. It introduces some overhead to blockchain
processing, but has several important benefits:

 1. For each client implementing the protocol, we know it’s the latest
 version from blockchain.
 2. There is no central entity responsible for maintaining or
 distributing updates, any such update is proposed under implicit or
 explicit agreement of the majority of stake and then distributed in
 a decentralized way.
 3. We do not rely upon clients updating the software on their PCs
 in time; this is done automatically, and updates are announced directly
 via the blockchain.
 4. If any security flaws are detected in some version of the CSL protocol
 or in some particular implementation, there would be a mechanism to
 distribute an update rapidly (but still under agreement of the majority
 of stake).

## Application Update: Sign and Announce

Here we consider ways to update the application securely. Protocol
updates are a separate issue which is covered in the relevant section
of this document.

For an update to be applied, at least the majority of stake should put
their signatures on it.

This approach seems to fit really naturally into the CSL model, as in a
PoS cryptocurrency every stakeholder is responsible for maintaining the
system proportionally to the relative size of their stake, and the blockchain
is maintained via consensus among stakeholders.

Software updates are a part of this maintenance process too, so the
stakeholders should agree on whether to consider this update trusted.

### Implicit Agreement

The fact that stakeholders are responsible for system updates does not
restrict us to the system where every single update requires a signature
from the majority of stake. We can introduce the concept of an implicit
agreement.

An update has to have at least T% of the stake signatures to be
published on the blockchain. It is not enough for stakeholders to sign
the update — they should vote either for or against it. The update is
considered confirmed and should be adopted by nodes if all of the
following conditions are met:

 + At least 50% of the stake voted for or against the software update.
 + The majority of the stake voted for the update.
 + It has been on the blockchain for U slots.

### Incorporation of Alternative Clients

IOHK will maintain a single official client. But there is also room for
third-party alternative clients maintained by the community. One requires
to collect enough signatures from stakeholders to publish their system
update, which may be not an "update", but a different client developed
from scratch, or a fork of the official client. As long as this update has
enough signatures from stakeholders, the network considers it trusted, and
it is updated via the same mechanisms as the official client.

## Application Update: Deliver and Apply

А list of HTTP mirrors ran by IOHK shall be sufficient for a start.

In process of time, we plan to maintain a Bittorrent-based or
Bittorrent-like solution to distribute updates. In general, P2P update
distribution is a crucial business requirement due to legal concerns.
It is to be decided which Bittorrent-like solution particular we will use.

Also, it’s interesting to note that the update itself does not require
a secure and trusted channel to be used for delivery, as it is signed
with some known in advance and trusted key (or set of keys).

Application updates are prepared with bsdiff and applied either directly
or via an installer. We're considering migrating to courgette in the
future.

## Protocol Update

First, we need to distinguish hard and soft protocol updates.

A soft fork proposes modifying blockchain consensus rules so that the
new version blocks are still compatible with old version clients.
A hard fork is one that doesn’t maintain backward compatibility with
the previous version.

BIP-99 provides an excellent criteria to distinguish between these two
types of fork:

### Softfork

A consensus fork wherein everything that was previously invalid remains
invalid while blocks that would have been previously considered valid become
invalid. A hashrate majority of miners can impose the new rules. They
have some deployment advantages like backward compatibility.

### Hardfork

A consensus fork that makes previously invalid blocks valid. Hard forks
require all users to upgrade.

In theory, a hard fork may lead to a situation when a network splits into two
parts, each maintaining a separate chain: one from the nodes that adopted
the latest system update, and another from the nodes that rejected to do
that. This means some blocks from the first part are considered invalid by
the other part, and vice versa).

Protocol version is a tuple `(Maj, Min, Alt)`:

 + Major version (2 bytes): to be changed rarely, changes are not
    backward-compatible and would produce a hard fork.
 + Minor version (2 bytes): integer to be adjusted for each update.
   + Changes should be backward-compatible in a sense that a block generated
      by the new version shall be somehow accepted by old version.
   + A particular block may contain addresses of unknown type. For each case
   like this, a concise workaround should be found in order not to affect
   stability and correctness of the system.
 + Alt version (1 byte): integer to manage several alternative clients
    proposing concurrent updates to the protocol version.

The protocol version is to be announced in the application update, and
is to be put later into each block created by updated software.

A major version change triggers a hard fork in the future.
A minor version change notifies the network that the subsequent
application update modifies the protocol managed by a
soft fork.

Alt version is to enable software providers to introduce
modifications to the protocol with some degree of concurrency.

## Soft fork updates

BIP-34 describes a strategy to handle a soft fork protocol version
adjustment. The 75% and 95% rules are useful. And also ”Treat transactions
with a version greater than 1 as non-standard (the official Satoshi
client will not mine or relay them).”
Deprecated in favor of BIP-09.

BIP-09 describes using a version not as an indefinitely adjusting
protocol version value, but as a dynamic set of current soft fork
updates, publicly known. E.g. for BTC, it’s maintained here.

This BIP is extremely useful for bitcoin. For a soft fork to be
regarded as applied, the majority of the software running the net should
be updated with new features. Our case is slightly different: we
distinguish verified software updates. Though it’s still may happen that
different nodes run different versions of the official client — or even
running alternative clients (also verified/approved by the stake).
The threshold is ≥1916 blocks (95% of 2016), or ≥1512 for testnet (75%
of 2016).

For CSL with its update system considering application updates, we don’t
need such a sophisticated mechanism as proposed by BIP-9
(versioning described in the previous sections should be sufficient for
tracking soft fork updates). But something like BIP-34 is needed
here.

There is a thin line between what we can do within a soft fork and what we
cannot:
1. No matter what we do — even the oldest verifier which was updated 5 years
ago should be able to take a valid chain ("valid" as for the latest version)
and find it valid as well (this is what they meant under "everything invalid
remains invalid" in BIP-99 fork taxonomy).
2. Any blocks issued by the software with an older version may be considered
invalid by software with a newer version.

Obviously, imposing rule 2 as it is may cause the network to be split into
two parts: one miner with 40% of hash power, or a stakeholder with a stake
large enough, would update and maintain their own chain, rejecting blocks
from others, but others would be still able to maintain their chain, rejecting
blocks from this miner (because blocks from this miner would chain upon the
older blocks).
There is a resolution rule: if 95% of the latest 2016 blocks have a newer
block version, the blocks with the older version are rejected.

It may seem not at all obvious why we would like to make some block
version invalid at some moment. For example, we have a chain functioning for
1000 blocks; then why would we decide to start rejecting blocks with an old
version at some point? (Apart from having some philosophical views that
a blockchain should share version of protocol more or less the same.)

The key insight here is that any new feature is actually a restriction
on what we previously had. For example, we have plain old transactions which
may contain either PublicKey-based addresses or Script-based ones.
Then eventually we decide to include a third type of an address (no matter
what the purpose is). Which strategy do we need for verifying a block
with a transaction with an address of unknown type? Obviously the only option
is not verifying this address.

And then imagine somebody proposing a transaction to this address, probably
doing that with an intention to secure funds from being spent until some
conditions are met — and then watching them spent in some other
transaction in a block with version 1. This is the point. We cannot make
use of a restriction without waiting for the network to start assuming
the old version to be deprecated (since we can only start rejecting blocks
when their version is deprecated).

Also, having `attributes` field, we may become attacked by someone
stating he uses higher version of the protocol, polluting attributes
with meaningless keys, and we would have to accept his block. For this
reason, the blocks with the version x.y.z are rejected prior to
application of the update with the protocol version x.y.z approved by
stake.

That said, we propose the following default rule for version stabilization:

 > if 95%  of the latest 2000 blocks contain the version `>= (maj, min,
 > alt)`, then reject all blocks with lower versions. Let’s call it the
 > soft fork stabilization point (or stabpoint).

So, gathering everything up:
 1. Once the update is approved, the protocol version (say v0.5) can be used.
 2. Before a soft fork is resolved (i.e. the resolution rule hit), the
   updated nodes should issue blocks with the new version v5, but treat
   and validate blocks of version v5 as blocks of version v4.
 3. Non-updated updated nodes accept new blocks:
   a. Before the soft fork is resolved, discarding as per v4.
   b. After the soft fork is resolved, as per v5 (i.e. allowing a block to
  have unknown attributes, and so on).

## Hard fork Updates

Hard forks are resolved using Modified Proof of Burn. As it is not
implemented yet, we omit this section from this document and will
publish it as a separate document.

# Implementation Overview

Implementation of the update system can be found in the
[Pos.Update](https://github.com/input-output-hk/cardano-sl/tree/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update)
family of modules. The general approach to implementation is the same as
in other subsystems of CSL, such as Txp, Ssc and Delegation. The update
system has the global state, stored in the database. The global state can be
unambiguously derived from the information that is in the blockchain. The local
state, sometimes referred to as “mempool”, is stored in the memory.
The mempool is used for data transfer and inclusion of transferred data into
blocks. The network protocol (built with standard [Inv/Req/Data
pattern](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Communication/Relay.hs))
is described in [Application-level
document](/protocols/csl-application-level/) with a binary protocol
described in [Binary protocols document](/protocols/binary-protocols/).

Currently, everything is done to add hard fork functionality via
software update and then perform a hard fork as described in research
section; soft forks (or software updates) are fully implemented.

## Fields Updatable with a Softfork

An
[`UpdateProposal`](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update/Core/Types.hs#L97-L108) contains
fields for changing some parameters used by Cardano SL (for instance, slot
duration). Specifically, `upBlockVersion` is used to signify that a proposal
performs such changes; if `upBlockVersion` is greater than the last used
block version, the changes from `upBlockVersionData` will be applied.

`upBlockVersionData` has the
type
[`BlockVersionData`](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update/Core/Types.hs#L131-L142). Its fields are described below:

  * `bvdScriptVersion` – a script language version used to validate script
    transactions. If the proposal increases `upBlockVersion`, it must also
    increase `bvdScriptVersion` by 1 (and can't leave it unchanged).

  * `bvdSlotDuration` – slot duration (in milliseconds).

  * `bvdMaxBlockSize` – block size limit (in bytes). A proposal can't
    increase the block size limit more than twofold compared to the previous
    limit.

The checks described above are done
in
[`verifyNextBVData`](https://github.com/input-output-hk/cardano-sl/blob/f77917a6a6a393bb3ef158500c147181fe21ed39/src/Pos/Update/Poll/Logic/Base.hs#L194-L221).

In addition, there are some fields that are unused right now, but will be used
in the future. Their meaning is briefly described below:

  * `bvdMaxTxSize` – transaction size limit (in bytes).

  * `bvdMpcThd` – eligibility threshold for MPC.

  * `bvdHeavyDelThd` – threshold for heavyweight delegation.

  * `bvdUpdateVoteThd` – portion of total stake necessary to vote for or
    against an update.

  * `bvdUpdateProposalThd` – number of slots after which an update is
    implicitly approved (unless it has more negative votes than positive).

  * `bvdUpdateImplicit` – number of slots after which an update is implicitly
    approved (unless it has more negative votes than positive).

  * `bvdUpdateSoftforkThd` – portion of total stake such that if total stake
    of issuers of blocks with some block version is bigger than this portion,
    this block version is adopted.

## Proposal Accumulation

Proposals are stored in a mempool or gathered from the blockchain in
order to figure out which proposal is adopted, and whether or not the current
node has to participate in voting. No matter whether a change in
proposal state comes from the network/mempool, or from loading
the blockchain, it is stored in the `PollModifier` data structure and applied
appropriately.

### Updating the Mempool

As nodes deserialize [payloads of update system
messages](/protocols/binary-protocols/#update-system), they modify
mempool as implemented
[here](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update/MemState/Functions.hs#L40).

## Interaction With the Database

In order to verify update system data, we have to get this data from the
global state (database). To provide such interface, a [well-documented
set of typeclasses is
presented](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update/Poll/Class.hs).
It is important that their implementation relies on functions found
in
[Pos.DB.GState.Update](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/DB/GState/Update.hs).

## Core Types

Core types are mentioned in the Binary protocols document. Those types
reflect the concepts from the research section in a straightforward way.
Please refer to the [core types
module](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update/Core/Types.hs)
for more information.

## Update Proposal Adoption

A very important part of implementation of the update mechanism is
the part that works with genesis blocks for epochs and applies updates.
This logic resides
[in this well-documented function](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update/Poll/Logic/Softfork.hs#L67).
We explain terminology related to this process below.

### `softforkResolutionThreshold` Predicate

`softforkResolutionThreshold` is a predicate (referred to as “threshold”
in the code) which, for an update proposal with version `v`, says that
it is a version of software ran by the network.

### Acceptable Proposal

A proposal is called “acceptable” if the following conditions are met:

 + It is correctly formed, including necessary cryptographic signatures.
 + Its version is greater or equal to the currently adopted proposal
   (see below).

### Confirmed Proposal

An acceptable proposal is called “confirmed” if it was voted for by the
majority of stake, but `softforkResolutionThreshold` predicate isn't yet
true for it.

### Adopted Proposal

A confirmed proposal is said to be “adopted” if its
`softforkResolutionThreshold` predicate is true for it. For each
blockchain state, there is exactly one adopted version. Blocks are
checked taking into consideration the currently adopted version.

## Download New Version

In the
[Pos.Update.Download](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/Pos/Update/Download.hs)
module, the following algorithms are implemented. Downloaded updates are
applied using a tool called
[launcher](https://github.com/input-output-hk/cardano-sl/blob/22360aa45e5dd82d0c87872d8530217fc3d08f4a/src/launcher/Main.hs)

### Download Confirmed Update

To download a confirmed update, we extract the update hash from
`ConfirmedProposalState`. We extract it depending on whether or not we're
using an installer on given platform. If the update hash is extracted
successfully, the “Download Update by Hash” algorithm to download and save
the confirmed update is invoked.

### Download Update by Hash

To download an update by hash, we loop through known update servers trying
to download the update with given hash using `httpLBS` from HTTP. Simple. In
the end, we will either have the update completely downloaded or server list
exhausted and an error reported.
