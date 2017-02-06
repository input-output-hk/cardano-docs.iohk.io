---
layout: default
title: Differences
permalink: /cardano/differences/
group: cardano
visible: true
---

[//]: # (Not reviewed at all)

# Clarified things

## Time, slots, and synchrony

In basic model of *Ouroboros* time is divided into discrete units
called *slots*. However, there are no details on how to obtain current
time securely and with enough precision.

In *cardano-sl* current time is obtained by querying predefined set of
NTP servers. **TODO**: write more details or throw away.

## Coin Tossing and Verifiable Secret Sharing

As *Ouroboros* paper suggests, a VSS scheme by Schoenmakers is used in
*cardano-sl*. One of the challenges in using VSS scheme is associating
public key used for signing with public key used for VSS scheme
(`VssPublicKey`). This is solved by introducing
`VssCertificate`s. This certificate is a signature given by signing
key for pair consiting of `VssPublicKey` and epoch, until which this
certificate is valid. Initially all stakeholders with enough stake
for participation in randomness generation have certificates. When new
stakeholder with enough stake appears or when existing certificate
expires, new certificate should be generated and submitted to the
network. `VssCertificate`s are stored in blocks. **TODO**: refer to
section or maybe write more details or throw away.

**TODO**: also write about openings and proofs (specifically, that
`PVSS.Secret` is opening and `PVSS.Proof` is proof which is part of
commitment), but I am not an expert in this area.

## Block generation time

In *Ouroboros* paper it's not stated explicitly when slot leader
should generate a new block and send it to the network: it can be done
in the beginning of slot, in the end of slot, in the middle of slot,
etc. In `cardano-sl` there is a special constant (`networkDiameter`)
which approximates maximal time necessary to broadcast block to all
nodes in the network. Block is generated and announced
`networkDiameter` seconds before the end of slot.

## Stake Delegation

Delegation scheme, as described in paper, doesn't explicitly state
whether proxy signing certificates should be stored within blockchain
(though there is a suggestion to store revocation list in
blockchain). Without storing proxy signing certificates in blockchain
it's barely possible to consider delegated stake in checking
eligibility threshold. On the other hand, if all certificates are
stored in blockchain, it may lead to blockchain bloat when big portion
of blocks will be occupied by proxy certificates. Submiting a
certificate is free, so adversary can generate as many certificates as
she wants.

In `cardano-sl` there are two types of delegation: heavyweight and
lightweight. There is a threshold on stake which one has to posses in
order to participate in heavyweight delegation. Proxy signing
certificates from heavyweighy delegation are stored within
blockchain. Contrary, lightweight delegation is available for
everybody, but certificates are not stored within blockchain and
aren't considered in checking eligibility threshold. As paper
suggests, *delegation-by-proxy* scheme is used.
**TODO**: describe in details or refer somewhere.

## Transactions, UTXO, Scripts

**TODO**: should it be here (in clarifications or in additions) at all?

# Modified things

## Leader Selection Process

In *Ouroboros* Leader Selection Process is described as flipping
a `(1 - p₁) … (1 - pⱼ₋₁) pⱼ`-biased coin to see whether `j`-th
stakeholder is selected as leader of given slot. Here `pⱼ` is
probability of selecting `j`-th stakeholder.

In `cardano-sl` it is implemented slightly differently. **TODO**: refer
to section or provide details here or throw away. Also, as paper
suggests, short (32-bits) seed is used for initializing PRG instead of
using `n ⌈log λ⌉` random bits.

## Commitments, openings, shares sending

Time of sending is randomized a little bit. **TODO**: should it be here?

## Multicommitments

In *Ouroboros* each stakeholder is presented as exactly one
participant of underlying VSS scheme. However, it's natural that
stakeholder with big stake is more important than stakeholder with
small stake with regards to secret sharing. For instance, if three
honest stakeholders control 60% of stake in total (each of them
controls 20%) and there are 40 adversary stakeholders each having 1%
of stake, then adversary has full control over secret sharing.

To overcome this problem, in *cardano-sl* each stakeholder is allowed
to send number of commitments proportional to their stake.

**TODO**: is it enough? If not, should I refer to section about
GodTossing or provide details here?

## Randomness generation failure

*Ouroboros* doesn't cover situation when commitments can't be
recovered. However, practical implementation should account for such
scenarios. *cardano-sl* implementation uses seed consisting of all
zeroes if there are no recovered commitments.

**NOTE**: probably it's bad solution. It was a quick hacak before
testnet launch to protect us from protocol stop.

**NOTE**: we wanted to use previous seed if, for instance, commitments
from more than half of stake can't be recovered. I don't know whether
we will do it and when. These notes will be removed before pushing to
master I suppose.

# Added features

## Update System

**TODO**: is it enough to put a link to page about US here?

## Security of P2P

**TODO**: should it be here? If yes, is it enough to only put a link?
It addresses attacks from **8 Attacks Discussion**.

# Missing things

## Input Endorsers

No.

## Incentive Structure

No.
