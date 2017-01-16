---
layout: default
title: Introduction
permalink: /
group: base
children: introduction
---
# Introduction to Cardano SL

[//]: # (@any)

[//]: # (This is an example of one-line Markdown Commentary.)
[//]: #   (For information about documentation guidelines)
[//]: #   (please refer to file FOR_TECH_WRITERS.md in the)
[//]: #   (project root.)

[//]: # (TODO: When we have a glossary, make sure that all the)
[//]: # (      terms mentioned in this document are hyperlinked)

![cardano](/img/cardano.png)

Cardano SL (or Cardano Settlement Layer) is a cryptographic
currency designed and developed by [IOHK](https://iohk.io/team). You can
think of this product as of Bitcoin reimagined with a freedom to fix
design flaws of Bitcoin. For those
who don't know what Bitcoin is, in the [next
section](#cryptocurrency-basics) we'll talk a little
bit about what a cryptocurrency is and why do cryptocurrencies matter.
If you have basic understanding of Bitcoin, you may
[skip](#what-makes-cardano-sl-special) the next
section. If you're eager to start working with Cardano SL, refer to
[Installation Guide]() and [Wallet Operation Guide]() to get started.

## Cryptocurrency Basics

[//]: # (@any)

Before giving a definition of a cryptocurrency, let's first talk about
why do we care about digital currencies in general and cryptographic
currencies in particular.

### Why Do We Care?

#### Speed

As opposed to conventional (also known as fiat), centrally banked
currencies such as Yen or American Dollar, digital currencies don't
require a banking system to move value. With this restriction lifted,
work with digital currencies is much faster than work with banking,
especially on the global scale. The transfer of sending 10USD from
Osaka to Denver no longer takes days when digital currency is
used. All transactions are done rapidly, no matter the distance. The
transfer also follows the same understandable rules within a
predictable amount of time, ensuring speed and reliability.

#### You Own Your Money
All that a commercial bank account owner is given is a promise of
being paid a certain amount of money within a reasonable amount of
time after receiving a payout request.  Of course, banking systems
also have limits to any volume of value being moved, rendering an
individual unable to withdraw or transfer large amounts quickly. In
case of cryptocurrencies, the person who holds a
special kind of information, called _a secret key_ can spend the money
at will. No other entity has a power to manipulate value that a user
has.

#### Pseudonymity

One can have as many cryptocurrency addresses as they wish, receiving
and spending money from different addresses as per their purpose. A
merchant running an E-Commerce shop can have a set of addresses to
receive money and issue refunds and a personal "wallet" for their own
needs. All of this is controlled from one interface and there is no need
to log in to several payment platforms, which in turn makes the
process very time efficient.

#### Security

Your money is as secure as the _secret key_ that allows spending it.
That means that storing your secret key in a safe on a laptop
disconnected from the Internet is equivalent of having banknotes in a
password-protected lock-box in a safe. Absolutely nobody can steal this
money even by carrying out a successful cyber-attack.

#### Extensibility

Using an approach known as [_side chains_](), general-purpose
cryptocurrencies (such as Cardano SL or Bitcoin) can enable
domain-specific cryptocurrencies, such as Ethereum Classic. This way,
any innovation developed via domain-specific
cryptocurrency, can have participants who hold value in a
general-purpose cryptocurrency. Examples of such
applications are identity management, gaming and gambling, verifiable
computations.

### What is a Cryptocurrency?

Cryptocurrency is a form of digital currency that uses cryptography to
manipulate value. Cryptography provides a way to generate
proof of genuine authenticity of any kind of information. This is called
_digital signing_. In cryptocurrencies we generate a ledger (a database
which answers a question "which address has how much money") by signing
and sending transactions into the network and receiving blocks of
confirmed transactions. Cryptocurrencies are normally decentralized,
meaning that many people from all over the globe participate in
ledger generation by running cryptocurrency nodes. Obviously, a
consensus about the state of the ledger has to be achieved. Two most
significant approaches for achieving such consensus are discussed in the
next section.

## What Makes Cardano SL Special?

[//]: # (v0.1.0.0)

There are a lot of similarities between Bitcoin and Cardano
SL, but also there are quite some differences between those two
cryptocurrencies.The most significant difference is that Bitcoin is a
Proof of Work type cryptocurrency, while Cardano SL makes use of a
Proof of Stake approach to reach consensus. This empowers honesty and
longevity of the participants.

### Purpose of a Consensus Algorithm

Consensus algorithms are used to produce new transaction blocks,
resulting in updated state of the ledger. Whenever someone publishes
a block of transactions, they (or rather, their node that runs the
cryptocurrency protocol) have to attach a proof that they have merited
it. Below two types of such proofs are discussed.

### Proof of Work and Mining

Proof of Work is the most common consensus algorithm type for
cryptocurrencies. It originated in Bitcoin and is the reason why this
currency works well. To generate Proof of Work, a computer has to
solve a challenge. The challenge is a computationally heavy problem
which is hard to solve, but the solution is easy to verify. When a
computer on a Proof of Work-based network finds a solution, it publishes
it along with the transactions that the computer observed while
cracking the problem. The owner of this computer collects the transaction
fees and reward for generating a block. The entire process is called
_mining_. Mining is very energy-consuming and it's increase is
analogous to competitiveness.

### Proof of Stake and Minting

Proof of Stake is a novel approach to block generation. IOHK scientists,
lead by [Prof. Aggelos Kiayias](https://iohk.io/team/aggelos-kiayias/)
have designed the first provably secure proof of stake algorithm called
Ouroboros. Ouroboros lies at the heart of Cardano SL. Research team has
published a
[whitepaper](https://iohk.io/research/papers/a-provably-secure-proof-of-stake-blockchain-protocol/)
which is a worthy read for anyone with background in cryptocurrency
theory. The core of Proof of Stake is that instead of wasting
electricity on cracking computationally heavy problems, a node is
selected to mint a new block, with a probability that is proportional
to the amount of coins a particular node has. A node that attempts to
generate a block is called "a stakeholder". You can read more about this process in [Proof
of Stake in Cardano SL]().

## Beyond Settlement Layer

[//]: # (<2017-02-20>)

Cardano SL is a "Layer" for a reason. It's the first component of
the Cardano Platform. Eventually it will be expanded with a Control Layer,
serving as a trusted computation framework to evaluate special
kind of proofs to ensure that a certain computation was carried out
correctly. In gaming and gambling, such systems are  useful for
verifying the honesty of random number generation and game
outcomes. Accompanied with [side chains]() it will allow to accomplish
such tasks as provably fair distribution of winnings in games. The
application of Control Layer lies well beyond gaming and gambling
applications. Identity management, credit system and more will be a
part of Cardano Platform. We are also aiming to evolve Cardano SL wallet
program called [Daedalus]() into a universal cryptocurrency wallet
with automated cryptocurrency trading and cryptocurrency-to-fiat
transactions.
