---
layout: default
title: Leader Selection in Cardano SL
permalink: /technical/leader-selection/
group: technical
visible: true
---
[//]: # (Reviewed at e1d0f9fb37a3f1378341716916f0321fb55698df)

# Leader Selection in Cardano SL

This guide describes slot-leader selection process.

## Follow the Satoshi

As mentioned [earlier](/cardano/proof-of-stake/#follow-the-satoshi),
Cardano SL uses Follow the Satoshi (FTS) algorithm to choose slot-leaders. Slot-leaders for the
current epoch (i.e. for each slot of the current epoch) are computed by FTS in the beginning of current epoch.
So genesis block contains a list of selected slot-leaders.

FTS uses a shared seed which is result of [Multi Party Computation (MPC)](/cardano/proof-of-stake/#multi-party-computation)
algorithm for previous epoch: in the result of MPC some nodes reveal their seeds, xor of these seeds is called shared seed.

Choose several random stakeholders (specifically, their amount is
currently hardcoded in 'Pos.Constants.epochSlots').

The probability that a stakeholder will be chosen as a slot-leader is proportional to the
number of coins this stakeholder holds. The same stakeholder can be picked more than once.

How the algorithm works: we sort all unspent outputs in a deterministic
way (lexicographically) and have an ordered sequence of pairs @(StakeholderId, Coin)@.
Then we choose several random 'i's between 1 and amount of satoshi in the system;
to find owner of 'i'th coin we find the lowest x such that sum of all coins in this list
up to 'i'th is not less than 'i' (and then 'x'th address is the owner).

With P2SH addresses, we don't know who is going to end up with funds sent to them.
Therefore, P2SH addresses can contain 'addrDestination' which specifies which addresses
should count as “owning” funds for the purposes of follow-the-satoshi.

