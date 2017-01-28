---
layout: default
title:  Handling Blocks and Headers
permalink: /protocols/handling-blocks-and-headers/
group: protocols
---

# Handling Blocks and Headers

This guide describes blocks handling logic, which is defined in [Pos.Block.Logic](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs) module.

## Concepts

We work with blocks and blocks' headers. Fundamentally, we can:

* create block,
* verify block,
* apply block,
* rollback block,

and:

* get block headers by different criteria,
* classify block headers.

## Block Creation

There are two kinds of blocks: **main** block and **genesis** block. We create main block with [`createMainBlock`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L29) function and genesis block with [`createGenesisBlock`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L28) function.

### Main Block Creation

We try to create a new main block on top of the best chain if possible. We actually can create new block if:

* we know genesis block for epoch from given slot id,
* last known block is not more than [`slotSecurityParam`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L55) blocks away from given slot id.

Value of [`slotSecurityParam`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Constants.hs#L103) (actually - number of slots) depends on maximum number of blocks which can be rolled back. This [maximum number](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Constants.hs#L98) is a security parameter from the protocol paper.

First of all, we have to check whether our software can create block according to current global state. If not - we just report that this software is obsolete. If yes, we have to [check slot id](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L633): it shouldn't be too big but should be bigger than slot id from the last known block. Then, if this condition is satisfied, we can [actually create new block](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L646).

### Genesis Block Creation

We create genesis block for current epoch when head of currently known best chain is [`MainBlock`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L76) corresponding to one of last [`slotSecurityParam`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Constants.hs#L103) slots of (i - 1)-th epoch.

First of all, we try to get leaders. If there's no leaders or not enough blocks for LRC (Leaders and Richmen Computation), we just report about error, otherwise we're trying to create [actually new genesis block](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L581). However, sometimes we [shouldn't create one](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L558). For example, we shouldn't do it [for 0-th epoch](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L560).

## Block Application

We apply blocks using [`applyBlocks`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L474) function. The sequence of the blocks should be definitely valid: we must have verified all predicates and data checks regarding blocks. **Important**: all blocks in that sequence must have the same epoch!

If everything is ok, we [actually apply blocks](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic/Internal.hs#L77):

* [apply US (Update System)](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Update/Logic/Global.hs#L60),
* [apply delegation](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Delegation/Logic.hs#L290),
* [apply transactions](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Txp/Logic.hs#L72).

Moreover, we can verify blocks before application (i.e. apply blocks only if they're valid). We use [`verifyAndApplyBlocks`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L404) function for it. If some error occurred during applying, two behaviour are possible:

1. all blocks applied inside this function will be rollbacked,
2. this function will try to apply as much blocks as it's possible.

## Block Rollback

You can think about rollback as about opposite of application: when we do rollback, we cancel all changes made by application. We use [`rollbackBlocks`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L491) function to do it.

We get the tip and the first block to rollback. If they don't match, we just report about an error. If they match, we [actually rollback the sequence of blocks](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic/Internal.hs#L107):

* [rollback delegation](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Delegation/Logic.hs#L327), erases mempool of certificates,
* [rollback US](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Update/Logic/Global.hs#L86),
* [rollback transactions](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Txp/Logic.hs#L224).

Of course, sometimes we cannot rollback blocks. For example, it's [impossible to rollback 0-th genesis block](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic/Internal.hs#L118).

## Block Headers Classification

Header can be classified as:

1. [valid](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L196),
2. [invalid](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L198),
3. [useless](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L197).

We use function [`classifyHeaders`](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L207) for it.

We treat headers as **useless** if:

* [newest hash is the same as our tip](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L219),
* [newest hash difficulty is not greater than our tip's](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L221),
* [couldn't find LCA, maybe db state updated in the process](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L223),
* [too big difficulty difference of tip and LCA](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L241).

We treat headers as **invalid** if there are [any errors in chain of headers](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L215) or if [last block of the passed chain wasn't found locally](https://github.com/input-output-hk/cardano-sl/blob/517a72801c0bbb11a34c8d6a6d528fff5f094471/src/Pos/Block/Logic.hs#L217).
