---
layout: default
title: Ouroboros Proof of Stake Algorithm
permalink: /cardano/proof-of-stake/
group: cardano
---
# Ouroboros Proof of Stake Algorithm

Ouroboros Proof of Stake Algorithm is the most important part of
protocol, the way that nodes reach consensus about the state of ledger.

Ouroboros is unique as it is the first blockchain protocol based on
proof of stake that is scientifically proved to be secure.

## Why Proof of Stake?

The most important thing about picking proof of stake algorithm over
proof of work, which is adopted by Bitcoin is the energy consumption
considerations. Running Bitcoin protocol is very a tremendously
expensive endeavor. It is estimated, that 3.8 American Households can be
powered for a day by the energy spent to generate one Bitcoin
transaction. The energy requirements for running Bitcoin protocol only
grow as more and more Bitcoin miners sink money into mining and
difficulty of the problem that their computers (mining rigs) are
cracking increases. This is why researchers did their best to
investigate alternative ways to reach consensus such as using so-called
BFT (Byzantine Fault Tolerant) consensus algorithms and Proof of Stake
algorithms. First significant work on Proof of Stake was conducted by
the team of Nxt cryptocurrency, however their protocol had significant
flaws and no formal verification.

## What is Proof of Stake?

In this section we explain what does “Proof” mean and what “Stake”
means, and then we put it together, explaining what “Proof of Stake”
means.

### Proof

“Proof” part of Proof of Stake is about having evidence that blocks of
transactions are legitimate.

### Stake

“Stake” means “the relative value held by addresses on the node”. When
we say “relative value”, what we mean is “take all the value held by
wallets on a particular node and divide it by the total value in the
system”.

## Proof of Stake

Rather miners pouring money into mining rigs running the protocol, in
order to participate in running the protocol in Proof of Stake
environment, we say that “slot leaders” generate blocks for the
blockchain. Anyone can become a slot leader, if the coin selection
algorithm would select a coin they own. We say that this blockchain is
self-referential that means that maintaining the blockchain relies on
the network participants themselves and on the network state. Nothing
except for the network state and network participants being online
matters for the sake of Proof of Stake.

## Follow the Satoshi

Let's elaborate a little bit on how slot leader gets selected. The
smallest, atomic, piece of value is called a “coin”. In Bitcoin, atomic
piece is called “Satoshi”, honoring the creator of Bitcoin, Satoshi
Nakamoto. Fundamentally, we can say that the ledger produces
distribution of coins. Follow the Satoshi is an algorithm that
verifiably picks a coin, provided randomness. When your coin gets
selected, you become slot leader and can listen to transactions
announced by others, make a block of those transactions, sign it with
your secret key and publish it to the network. Of course, you don't have
to do it manually, your node will take care of everything.

## Multi Party Computation

The matter of fueling Follow the Satoshi with randomness is another
problem in itself. We're using Multi Party Computation approach when
select nodes provide so called “commitments” and then those get
“revealed”, producing a random value generated independently by
participants of the network.
