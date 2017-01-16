---
layout: default
title: Transactions in Cardano SL
permalink: /cardano/transactions/
group: cardano
---
# Transactions in Cardano SL

[//]: # (<2017-02-21>)

You can think of transactions in Cardano SL as entities that consist of
list of inputs and a list of outputs. Outputs of a transaction can later
be used as inputs for another transaction.

Every node (except for SPV, or lightweight nodes) in the network
verifies transactions, so those nodes have to keep track of unspent
outputs, this is called “utxo”, or “Unspent Transaction Outputs”. By
tracking utxo, every node can validate that inputs in a published
transaction are indeed unspent outputs.

Transactions are signed with the issuer's secret key. All of this,
including selection of unspent outputs to be used as inputs is done
automatically and you don't have to worry about it when making a
transaction using Daedalus UI.

## Proofs of Transaction Legitimacy

Each transaction in Cardano is accompanied by a proof (also called a
witness) that this transaction is legitimate. Those proofs are stored on
the blockchain and anybody can see, inspect and independently verify
them. However, not everybody needs to do that – for instance, if you are
an SPV node, you are trusting other nodes to do the verification, and
thus you would rather not have those proofs sent to you with every
transaction, as they are useless to you. Moreover, even if you are a
full node, you might want to delete old proofs after some time in order
to save space.

The technique of storing transactions separately from their proofs is
called “segregated witness” (you may have heard of it being recently
implemented in Bitcoin). Under this scheme, transactions and proofs are
stored in two separate places in a block, and can be processed
independently.
