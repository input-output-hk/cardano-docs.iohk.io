---
layout: default
title:  Handling Blocks and Headers
permalink: /protocols/handling-blocks-and-headers/
group: protocols
---

# Handling Blocks and Headers

This guide describes blocks handling logic, which is defined in [Pos.Block.Logic](https://github.com/input-output-hk/cardano-sl/blob/master/src/Pos/Block/Logic.hs) module.

## Concepts

We work with blocks and blocks' headers. Fundamentally, we can:

* create block,
* apply block,
* rollback block,

and:

* get block headers by different criteria,
* classify block headers.

## Block Creation

There are two kinds of blocks: **main** block and **genesis** block. We create main block with `createMainBlock` function and genesis block with `createGenesisBlock` function.

_Pending_

## Block Applying

_Pending_

We can verify block before applying.

## Block Rollback

You can think about rollback as about opposite of applying: when we do rollback, we cancel all changes made by applying.

_Pending_

## Block Headers Classification

Header can be classified as:

1. **Valid**.
2. **Invalid**.
3. **Useless**.

We use function `classifyHeaders` to classify headers.
