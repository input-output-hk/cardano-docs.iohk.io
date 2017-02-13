---
layout: default
title:  Handling Blocks and Headers
permalink: /protocols/handling-blocks-and-headers/
group: protocols
---

[//]: # (Reviewed at 60033350e60408fc79f202491e6985b3b47acd90)

# Handling Blocks and Headers

This guide describes blocks handling logic, which is defined in
[Pos.Block.Logic](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs)
module.

## Concepts

We work with blocks and block headers. Fundamentally, we can:

-   create a block,
-   verify a block,
-   apply a block,
-   rollback a block,

and:

-   get block headers by different criteria,
-   classify block headers.

## Block Creation

There are two kinds of blocks: a **main** block and a **genesis** block. A main
block is created with the
[`createMainBlock`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L29)
function and a genesis block is created with the
[`createGenesisBlock`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L28)
function.

### Main Block Creation

We try to create a new main block on top of the best chain if possible. A new
block can be created if the following conditions are met:

-   We know the genesis block for the epoch from the given slot ID,
-   The last known block is not more than
    [`slotSecurityParam`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L55)
    blocks away from given slot id.

The value of
[`slotSecurityParam`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Constants.hs#L103)
(which actually is a number of slots) depends on maximum number of blocks which
can be rolled back. This [maximum
number](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Constants.hs#L98)
is a security parameter from the protocol paper.

First of all, we have to check whether our software can create a block according
to current global state. If it can not, we just report that this software is
obsolete. If it can, we have to [check slot
ID](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L633):
it shouldn’t be too large but should be larger than the slot ID from the last
known block. Then, if this condition is met, we can [actually create the new
block](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L646).

### Genesis Block Creation

We create a genesis block for the current epoch when the head of currently known
best chain is
[`MainBlock`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L76)
corresponding to one of the last
[`slotSecurityParam`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Constants.hs#L103)
slots of (i - 1)-th epoch.

First of all, we try to get the leaders. If there’s no leaders or not enough
blocks for LRC (Leaders and Richmen Computation), an error is reported,
otherwise we’re trying to actually create [a new genesis
block](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L581).
However, sometimes we [shouldn’t create
one](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L558).
For example, we shouldn’t do it [for the 0-th
epoch](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L560).

## Block Application

We apply blocks using the
[`applyBlocks`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L474)
function. The sequence of blocks should be definitely valid: we must verify all
predicates and data checks regarding blocks. **Important**: all blocks in that
sequence must be of the same epoch!

If everything is ok, we [actually apply
blocks](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic/Internal.hs#L77):

-   [apply US (Update
    System)](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Update/Logic/Global.hs#L60),
-   [apply
    delegation](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Delegation/Logic.hs#L290),
-   [apply
    transactions](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Txp/Logic.hs#L72).

Moreover, we can verify blocks before application (i.e. apply blocks only if
they’re valid). We use
[`verifyAndApplyBlocks`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L404)
function for it. If some error occurred during application, there are two
options:

1.  All blocks applied inside this function will be rollbacked.
2.  This function will try to apply as much blocks as it’s possible.

## Block Rollback

You can think about a rollback as the opposite of application: when a rollback
is performed, all changes made by the application are cancelled. To do this, the
[`rollbackBlocks`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L491)
function is used.

We get the tip and the first block to rollback. If they do not match, an error
is reported. If they match, we [actually rollback the sequence of
blocks](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic/Internal.hs#L107):

-   [Rollback
    delegation](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Delegation/Logic.hs#L327),
    erases mempool of certificates,
-   [Rollback
    US](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Update/Logic/Global.hs#L86),
-   [Rollback
    transactions](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Txp/Logic.hs#L224).

Of course, sometimes we cannot rollback blocks. For example, it’s [impossible to
rollback 0-th genesis
block](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic/Internal.hs#L118).

## Block Headers Classification

A header can be classified as:

1.  [Valid](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L196),
2.  [Invalid](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L198),
3.  [Useless](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L197).

The function
[`classifyHeaders`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L207)
is used for it.

A header is treated as **useless** if the following conditions are met:

-   [The newest hash is the same as our
    tip](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L219),
-   [The newest hash difficulty is not greater than our
    tip’s](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L221),
-   [LCA couldn’t be found, maybe db state was updated during the
    process](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L223),
-   [The difference between the tip and LCA is too
    big](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L241).

A header is treated as **invalid** if there are [any errors in the chain of
headers](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L215)
or if [the last block of the passed chain wasn’t found
locally](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L217).
