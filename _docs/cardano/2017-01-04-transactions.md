---
layout: default
title: Transactions in Cardano SL
permalink: /cardano/transactions/
group: cardano
---
[//]: # (Reviewed at 403cea2d897aba95163b709bd13c35d343116f3f)

# Transactions in Cardano SL

## Overview

A transaction (*tx*) is a special data represents the *act* of the value
transferring between nodes (from the user’s point of view - between wallets).
Thus, when the user *Alice* sends money to the user *Bob*, the new transaction
emerges. Let’s call this transaction `Tx1`, the node under *Alice*’ wallet `N1`,
and the node under *Bob*’s wallet `N2`.

So, the node `N1` do these steps:

1.  [creates](https://github.com/input-output-hk/cardano-sl/blob/63adb31e813e21ec9da21cfa69984840308bbfa2/src/Pos/Wallet/Tx.hs#L41)
    transaction `Tx1` and
    [signs](https://github.com/input-output-hk/cardano-sl/blob/63adb31e813e21ec9da21cfa69984840308bbfa2/src/Pos/Wallet/Tx/Pure.hs#L83)
    it with its private key,
2.  [sends](https://github.com/input-output-hk/cardano-sl/blob/63adb31e813e21ec9da21cfa69984840308bbfa2/src/Pos/Wallet/Tx.hs#L53)
    it to all known nodes (i.e. neighbors),
3.  [save](https://github.com/input-output-hk/cardano-sl/blob/63adb31e813e21ec9da21cfa69984840308bbfa2/src/Pos/Wallet/Tx.hs#L44)
    it in its local data.

Each of `N1`’s neighbors sends transaction `Tx1` to its neighbors and so on, and
some slot leader will store this transaction to the some block in the ledger.
Please note that if the network is under high load, it may take a lot of time
for transaction to be actually added in the block.

## Design

Each transaction contains a list of *inputs* and a list of *outputs*, and
outputs of the transaction `Tx0` can be used as an inputs of the other
transaction `Tx1` and so on:

                Tx0                           Tx1
      +----------------------+      +----------------------+
      |                      |      |                      |
      |  Inputs     Outputs  |      |  Inputs     Outputs  |
      | +------+   +-------+ |      | +------+   +-------+ |
      | | In0  |   | Out0  + |      | | In0  |   | Out0  | |
      | +------+   +-------+ |      | +------+   +-------+ |
      | | In1  |   | Out1  | |      | | In1  |   | Out1  | |
      | +------+   +-------+ |      | +------+   +-------+ |
      | | ...  |   | ...   | |      | | ..   |   | ...   | |
      | +------+   +-------+ |      | +------+   +-------+ |
      | | InN  |   | OutM  | |      | | InN  |   | OutM  | |
      | +------+   +-------+ |      | +------+   +-------+ |
      |                      |      |                      |
      +----------------------+      +----------------------+     ...

Inputs and outputs inform us about *money flow*: input informs where the money
came from, and output informs where the money come to. Please notice that
there’s `N` and `M`, because actual number of inputs and outputs can be
different.

So, each input contains:

1.  An id of transaction `TxN` which output is used for this input.
    Transaction’s id is a BLAKE2s-256 hash of the transaction, something like
    `f9bcbe752aee4512457f1fd350200cf870906b7e6e836688c9a3779645c86c01`.
2.  An index of the using output in `TxN`’s outputs.

And each output contains:

1.  An address of the node `N` we want to send a value to. An address is a
    BLAKE2s-224 hash of the hash of the public key of the `N` node, something
    like `88ca7f79d4edcf911b60eeb96c8e9284d0c07c6e61c59a9e1c17a5e9`. Please read
    about [Addresses in Cardano SL](/cardano/addresses/) for more info.
2.  Amount of money we want to send. This value is 64-bit unsigned integer with
    [hardcoded](https://github.com/input-output-hk/cardano-sl/blob/63adb31e813e21ec9da21cfa69984840308bbfa2/src/Pos/Types/Core.hs#L88)
    maximum value.

For example:

      Tx 891971a4cc31e32..                           Tx f9bcbe752aee4512..
    ------------------------+           +----------------------------------------------+
    \                       |           |                                              |
    /        Outputs        |           |       Inputs                  Outputs        |
    \  +------------------+ |           | +-----------------+     +------------------+ |
    /  | Out0             | |           | | In0             |     | Out0             | |
    \  | +--------------+ | |           | | +-------------+ |     | +--------------+ | |
    /  | | Value        | | |           | | | Tx id       | |     | | Value        | | |
    \  | | 100 ADA      | | |           | | | 891971a4c.. | |     | | 100 ADA      | | |
    /  | +--------------+------->>  ------>>+-------------+ |     | +--------------+------->>
    \  | | Node address | | |           | | | Out index   | |     | | Node address | | |
    /  | | a00e4bb2..   | | |           | | | 0           | |     | | 88ca7f79..   | | |
    \  | +--------------+ | |           | | +-------------+ |     | +--------------+ | |
    /  | ...              | |           | | ...             |     | ...              | |
    \  +------------------+ |           | +-----------------+     +------------------+ |
    /                       |           |                                              |
    ------------------------+           +----------------------------------------------+

Node `a00e4bb2..` generates transaction `f9bcbe752aee4512..`, and this
transaction informs us that:

1.  we want to send 100 ADA from the current node with address `a00e4bb2..` to
    the node with address `88ca7f79..`;
2.  this money corresponds to `0`-th output of the previous transaction with an
    id `891971a4c..`.

## Verification

As said above, transaction’s output become an input of the other transaction. In
this case we treat such an output as *spent transaction output*. Thus, an output
`Out0` of the transaction `891971a4cc31e32..` is spent output because it already
became an input of the transaction `f9bcbe752aee4512..`.

But such a spending doesn’t occur immediately, so an output which *isn’t yet* an
input of another transaction called *unspent transaction output*. Only unspent
output can be used as an input for the other transaction, to prevent
[double-spending](https://en.bitcoin.it/wiki/Double-spending).

So every node in the network not just accepts transactions, but
[verify](https://github.com/input-output-hk/cardano-sl/blob/63adb31e813e21ec9da21cfa69984840308bbfa2/src/Pos/Types/Tx.hs#L91)
them. To do it, every node have to keep track of unspent outputs, it allows to
validate that inputs in a published transaction are indeed the unspent outputs.
Actually, all unspent outputs called *utxo*, and this is a part of the special
key-value database called *Global State*.

## Proofs of Transaction Legitimacy

Each transaction in Cardano SL is accompanied by a proof (also called a
[witness](https://github.com/input-output-hk/cardano-sl/blob/63adb31e813e21ec9da21cfa69984840308bbfa2/src/Pos/Types/Types.hs#L93))
that this transaction is legitimate. Even if the output is an unspent one, we
have to prove that we have *a right* to spend it. Since a transaction `TxN` can
have many inputs, witness for it consists of the witnesses of all `TxN`’s
inputs, and only if all the inputs are legitimate, `TxN` is legitimate too. If
particular transaction isn’t legitimate - it will be rejected by the network.

Because of [two available types of node
address](/cardano/addresses/#what-does-an-address-look-like) we use two
coresponding variants of the witness: based on *public key* and based on
*script*.

For example, first variant works with a public key `PK` and transaction
signature: legitimate input must be [signed
with](https://github.com/input-output-hk/cardano-sl/blob/63adb31e813e21ec9da21cfa69984840308bbfa2/src/Pos/Wallet/Tx/Pure.hs#L81)
a private key corresponding to `PK`. So it’s possible to [check this
signature](https://github.com/input-output-hk/cardano-sl/blob/63adb31e813e21ec9da21cfa69984840308bbfa2/src/Pos/Types/Tx.hs#L231)
and either accept that input or reject it.

Witnesses are stored in the blockchain and anybody can see, inspect and
independently verify them. But after some time node can delete old proofs in
order to save space. The technique of storing transactions separately from their
proofs is called “segregated witness” (you may have heard of it being recently
[implemented in
Bitcoin](https://bitcoincore.org/en/2016/01/26/segwit-benefits/)). Under this
scheme, transactions and proofs are stored in two separate places in a block,
and can be processed independently.
