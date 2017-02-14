---
layout: default
title: Addresses
permalink: /cardano/addresses/
group: cardano
---
[//]: # (Reviewed at 403cea2d897aba95163b709bd13c35d343116f3f)

# Addresses in Cardano SL

To send and receive value, addresses are used in virtually all cryptocurrencies.
Cardano [supports](https://github.com/input-output-hk/cardano-sl/blob/f37c6cf6a43f42cd7c0a0477e33ae95155d50450/src/Pos/Types/Core.hs#L231)
two types of addresses: `PubKeyAddress` and `ScriptAddress`.

`PubKeyAddress` is a normal address like in any other cryptocurrency. It is nothing but a
hashed [public key](https://github.com/input-output-hk/cardano-sl/blob/f37c6cf6a43f42cd7c0a0477e33ae95155d50450/src/Pos/Types/Core.hs#L231).
[Read more about public key addresses below](#public-key-addresses).

`ScriptAddress` is used in so-called "Pay to Script Hash" (P2SH) transactions. It operates
autonomously and acts somewhat like a bank deposit: you can send money to it, but
in order to redeem it you have to satisfy certain conditions, determined by a
[script](https://github.com/input-output-hk/cardano-sl/blob/f37c6cf6a43f42cd7c0a0477e33ae95155d50450/src/Pos/Script/Type.hs#L38)
associated with the address. The address itself contains the hash of the serialized script.
[Read more about P2SH below](#pay-to-script-hash).

## What Does an Address Look Like?

Addresses are `base58`-encoded bytestrings, consisting of:

* 1 byte: address type;
* 28 bytes: hash of some data structure (different for each type);
* 4 bytes: CRC32 checksum.

All addresses are 33 bytes long.

`Base58` is the same encoding as used in Bitcoin. It uses a 58-symbol alphabet
to encode data, hence the name. Here is the alphabet we are using:

    123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz

It avoids both non-alphanumeric characters and letters which might look
ambiguous when printed (`0`, `O`, `I`, `l`); therefore it is suitable for
human users who enter the data manually, copying it from some visual source,
and also allows easy copy and paste because a double-click will usually select
the whole string.

Currently there are only two types of addresses in Cardano: `PubKeyAddress`
and `ScriptAddress`. Here are the `type`s for each:

| `type`  | Address type    |
|---------|-----------------|
| [0](https://github.com/input-output-hk/cardano-sl/blob/2f3c7df7d324bc056fefe0fce856e39a692f6d9f/src/Pos/Binary/Address.hs#L18)       | `PubKeyAddress` |
| [1](https://github.com/input-output-hk/cardano-sl/blob/2f3c7df7d324bc056fefe0fce856e39a692f6d9f/src/Pos/Binary/Address.hs#L22)       | `ScriptAddress` |
| [arbitrary number](https://github.com/input-output-hk/cardano-sl/blob/2f3c7df7d324bc056fefe0fce856e39a692f6d9f/src/Pos/Binary/Address.hs#L26) | `UnknownAddressType` |

For hashing, we use a combination of `SHA3-256` and `BLAKE2s-224`, i.e.:

    address_hash(x) = BLAKE2s_224(SHA3_256(x))

See sections on [`PubKeyAddress`](#public-key-addresses) and
[`ScriptAddress`](#pay-to-script-hash) for a description of
what `x` is in each case.

We also adopt a way to make sure that an address is entered correctly
by appending a 32-bit Cyclic Redundancy Code checksum (`CRC32`) to
the end of the address. This way, the full address is
[generated](https://github.com/input-output-hk/cardano-sl/blob/2f3c7df7d324bc056fefe0fce856e39a692f6d9f/src/Pos/Binary/Address.hs#L50)
with the following rule, where `+` is concatenation:

    address' ← type + address_hash(x)
    address ← toBase58(address' + crc32(address'))

Here is an example of a valid address:

    1EWYSJnvgnSUmp8Gi4mADvU2zkJgVAA7McgFRXiqwDBs8

which can be decoded into the following byte string (with spaces separating
type, hash and checksum):

    00 C8B9519459F5D4E42B002EF06AE94DC9C0A5B87E52D0D0375FD83ECE C52CEB43

## Public Key Addresses

As mentioned in the [Introduction](/#you-own-your-money), the
wallets you see in the user interface are a convenient representation of
the fact that you own a secret key to spend money in this particular
wallet. But how is such spending verified by the network and how can you
receive money from others? The answer is that along with the secret key
which is used to control the value in your wallets, a public key is
generated. This public component can be known by anybody, hence the name.

A `PubKeyAddress` contains the hash of this public key.

Public keys are also used for verifying your identity when your create a
transaction and other auxiliary purposes.

To sum up, a public key address represents your personal wallet. It is
constructed as

    address' ← 0x00 + address_hash(public_key)
    address ← toBase58(address' + crc32(address'))

## Pay to Script Hash

The idea of P2SH is to provide a lot of flexibility to formulating complex
rules for spending money. Instead of sending a transaction to a public key
address, we create a validator script that can take a so-called redemption script
as a parameter. To redeem funds, we pass the redemption script to the
validator and evaluate it. If it evaluates to `success`, money is sent as
specified by the redeemer. Otherwise nothing happens.

To quote Bitcoin Wiki,

> Using P2SH, you can send bitcoins to an address that is secured in
> various unusual ways without knowing anything about the details of how
> the security is set up. The recipient might need the signatures of
> several people to spend these bitcoins, or a password might be
> required, or the requirements could be completely unique.

`ScriptHash` addresses are constructed as follows:

    address' ← 0x01 + address_hash(serialize(validator_script))
    address ← toBase58(address' + crc32(address'))

## Other address types

In the future, we may use the update system to introduce other address types
with different values in the `type` field.
[See more](/cardano/update-mechanism/#soft-fork-updates) on extending the system
in non-breaking fashion.
