---
layout: default
title: Wallets in Cardano SL
permalink: /cardano/wallets/
group: cardano
---

# Wallets in Cardano SL

While addresses, discussed in [Addresses](/cardano/addresses/), are
fundamental to send and receive funds, wallets are a way to simplify these
processes for the end-user.

## What is a Wallet?

In Cardano, wallets are defined in the following manner:

~~~ haskell
data CWallet = CWallet
	{ cwAddress :: !CAddress
	, cwAmount  :: !Coin
	, cwMeta    :: !CWalletMeta
	}
~~~

Where `CWalletMeta` is a type that, presently, indicates whether the wallet is
shared or personal, the currency that this wallet uses, and the wallet's name.
With this, the wallet type is easily extensible as any additional features can
be added to the `CWalletMeta` type, leaving the `Address` and `Coin` fields
untouched, which every wallet, regardless of name, type and currency, must
have.

## Transactions and Wallets

In the [Transactions](/cardano/transactions/) section, the structure of
transactions is defined. However, to facilitate client operations, transactions
are represented differently in clients. They are represented as

~~~ haskell
data CTx = CTx
	{ ctId            :: CTxId
	, ctAmount        :: Coin
	, ctConfirmations :: Word
	, ctType          :: CTType -- it includes all "meta data"
	}
~~~

Essentially, a client transaction is composed by the actual transaction's `Id`,
by the amount the wallet in question received, the number of confirmations this
transaction has received (i.e., the number of blocks currently on top of the
block containing the transaction in question), and a label indicating whether
the transaction is incoming or outcoming. Inside the `CTType` datatype, there
is, similarly to `CWallet`, a datatype with metainformation concerning the
transaction. Aside from a label indicating whether it is ingoing or outgoing,
this metainformation - the datatype `CTxMeta` - indicates the transaction's
currency, its title or name, its description and the POSIX-formatted date at
which it was sent.

## Wallet API

Currently, the wallet's API provides a series of methods with which to work
with wallets. The `servant` Haskell library was used, which provides a modular
approach to API-building - it uses combinators to both build atomic HTTP
actions and to glue these atomic methods together to form larger and more
complete APIs.

If the event requests fail, there is a `WalletError` type, which
is simply a wrapper over `Text` to show what happened.

Currently, the wallet's API supports the following operations:

* Given an address, fetch the wallet related to that address, if it exists.
* Fetch all wallets to which the system has access to.
* Given origin and destination addresses and an amount of coins, send the coins in the default currency (presently, `ADA`) from the origin address to the destination one, without a transaction message or description.
* Given origin and destination addresses, an amount of coins, a currency, a title and a description, send the coins of the given currency from the origin address to the destination one (Note that both of the previous methods do not presently support many-to-many
transactions).
* Given an address, a number of transactions to skip and a limit of transactions to take, fetch a tuple with the list of transactions in which the address took part in, in the index interval [skip + 1, limit], and its length. The transactions are in ascending order of age, i.e. newer transactions come first.
* Given an address, a `Text` string, a number of transactions to skip and a limit of transactions to take, fetch a tuple with the list of transactions whose title has the string as an infix, in the index interval [skip + 1, limit], and its length. The transactions are in ascending order of age, i.e. newer transactions come first.
* Given an address and a transaction's ID, and a transaction's metainformation - the datatype `CTxMeta` - add the transaction which has the given ID to the wallet's transaction history, if such a tranasction exists.
* Create a new wallet.
* Given a value of type `CWalletInit` - which consists of a wallet's backup phrase and a wallet's metainformation (`CWalletMeta`), recover the wallet associated to the given information, if it exists.
* Given an address, delete the wallet to it associated. Returns no data to the client, aside from an eventual error.
* Given a currency and an address, answer the request with a `Bool`, with `True` meaning the address was valid, and `False` otherwise. Presently, any currency other than `ADA` results in a `False`.
* Fetch the client's current user profile - the datatype `CProfile`, which has several fields, among them: the user's name, their email, their phone number, the hash of their password, the `POSIX`-formatted time at which the account was created, their location, and the account's picture.
* Given a value of type `CProfile`, update the user's profile, returning the new one in the process.
* Given an `ADA` redeeming token - the datatype `CWalletRedeem`, which has the address into which the `ADA` intended to be redeemed will go to, and a redemption seed - create and return a wallet with the redeemed `ADA`.
* Fetch information related to the next update.
* Apply the system's most recent update.
* Fetch the value of current slot duration.
* Fetch the system's version.
