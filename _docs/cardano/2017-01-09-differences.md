---
layout: default
title: Differences Between the Paper and the Implementation
permalink: /cardano/differences/
group: cardano
visible: true
---
[//]: # (Reviewed at 86f679863c67c2aaf23c37cf2c23b9101fd1c77d)

# Differences Between Paper and Implementation

This document is a work-in-progress. The goal of this document is to
enumerate all the ways in which CSL implementation differs from the
specifications presented in the paper.

## Time, Slots, and Synchrony

In a basic model of *Ouroboros* time is divided into discrete units
called *slots*. However, there are no details on how to obtain current
time securely and with enough precision.

In *cardano-sl* current time is obtained by querying a predefined set of
NTP servers.

## Coin Tossing and Verifiable Secret Sharing

As *Ouroboros* paper suggests, a VSS scheme by Schoenmakers is used in
*cardano-sl*. One of the challenges while using a VSS scheme is associating
the public key used for signing with the public key used for VSS scheme
(`VssPublicKey`). This is solved by introducing
`VssCertificate`s. This certificate is a signature given by signing
key for a pair consisting of `VssPublicKey` and the epoch until which this
certificate is valid. Initially, all stakeholders with stake enough
for participation in randomness generation have certificates. When a new
stakeholder with enough stake appears or when an existing certificate
expires, a new certificate should be generated and submitted to the
network. `VssCertificate`s are stored in blocks.

## Block Generation Time

In *Ouroboros* paper, they do not state explicitly when a slot leader
should generate a new block and send it to the network: it can be done
in the beginning of a slot, in the end of a slot, in the middle of a slot,
etc. In `cardano-sl` there is a special constant (`networkDiameter`)
which approximates maximal time necessary to broadcast a block to all
nodes in the network. A block is generated and announced
`networkDiameter` seconds before the end of a slot.

## Stake Delegation

Delegation scheme, as described in paper, doesn't explicitly state
whether proxy signing certificates should be stored within the blockchain
(though there is a suggestion to store the revocation list in the blockchain).
Without storing proxy signing certificates in the blockchain
it's barely possible to consider delegated stake in checking
eligibility threshold. On the other hand, if all certificates are
stored in the blockchain, it may lead to a blockchain bloat when a big portion
of blocks will be occupied by proxy certificates. Submitting a
certificate is free, so adversary can generate as many certificates as
she wants.

There are two types of delegation in `cardano-sl`: heavyweight and
lightweight. There is a threshold on stake that one has to posses in
order to participate in heavyweight delegation. Proxy signing
certificates from heavyweight delegation are stored within the
blockchain. On the contrary, lightweight delegation is available for
everybody, but certificates are not stored within the blockchain and
aren't considered when checking eligibility threshold. As paper
suggests, *delegation-by-proxy* scheme is used.

# Modified Things

## Leader Selection Process

In *Ouroboros* Leader Selection Process is described as flipping
a `(1 - p₁) … (1 - pⱼ₋₁) pⱼ`-biased coin to see whether `j`-th
stakeholder is selected as leader of given slot. Here `pⱼ` is
probability of selecting `j`-th stakeholder.

In `cardano-sl` it is implemented in a slightly different way.

## Commitments, openings, shares sending

Time of sending is randomized within ε.

## Multicommitments

In *Ouroboros* each stakeholder is presented as exactly one
participant of the underlying VSS scheme. However, it's natural that
a stakeholder with more stake is more important than a stakeholder with
less stake with regards to secret sharing. For instance, if three
honest stakeholders control 60% of stake in total (each of them
controls 20%) and there are 40 adversary stakeholders each having 1%
of stake, then adversary has full control over secret sharing.

To overcome this problem, in *cardano-sl* each stakeholder is allowed
to send number of commitments proportional to their stake.

## Randomness Generation Failure

*Ouroboros* doesn't cover the situation when commitments can't be
recovered. However, practical implementation should account for such
scenarios. *cardano-sl* implementation uses a seed consisting of all
zeroes if there no commitments could be recovered.

# Added Features

## Update System

See the article on [update system](/cardano/update-mechanism/).

## Security of P2P

See the article on [P2P implementation and hardening](/protocols/p2p/).

# Missing Things

The sections on _Input Endorsers_ and _Incentive Structure_ aren't
implemented yet. Those sections are to be implemented together with
the pending research on Side-chains and released within the Side-chains release.
