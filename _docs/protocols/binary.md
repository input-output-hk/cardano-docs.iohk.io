---
layout: default
title: Binary protocols
permalink: /protocols/binary-protocols/
group: protocols
---

# Binary protocols

Sizes of all fields are represented in bytes. Big-Endian is used
everywhere. Composite types are serialized in the order of definition
with no delimiters.

For example, `(Word32, Word8)` is serialized with five bytes — four,
for `Word32`, and 1 for `Word8`.

For variable-length structures, dependant on object of type T, we use
`size(T)` notation.

To test serialization of objet `myObject` in `ghci`,
one should use the following command:

```
ghci> toLazyByteString $ lazyByteStringHex $ encode $ myObject
```

## Common datatypes

### Coin

~~~
-- | Coin is the least possible unit of currency.
newtype Coin = Coin
    { getCoin :: Word64
    } deriving (Show, Ord, Eq, Bounded, Generic, Hashable, Data, NFData)
~~~

| Field size | Type   | Field name | Description            |
| ---------- |--------| ---------- |------------------------|
| 8          | Word64 | getCoin    | 64bit unsigned integer |

Example: `Coin 31 --> 0x000000000000001F`

### Hash

~~~
-- | Hash wrapper with phantom type for more type-safety.
-- Made abstract in order to support different algorithms in
-- different situations
newtype AbstractHash algo a = AbstractHash (Digest algo)
    deriving (Show, Eq, Ord, ByteArray.ByteArrayAccess, Generic, NFData)

-- | Type alias for commonly used hash
type Hash = AbstractHash Blake2s_224
~~~

| Field size | Type      | Description             |
| ---------- | --------- | ----------------------- |
| 28         | Word8[28] | 224 bits of hash digest |


So whenever you see `Hash SomeType` in the code, this field will occupy 28
bytes.
Additional type parameter after `Hash` is used only in code for
type-safety and has no impact on serialization.

Example:

~~~
ghci> hash $ mkCoin 3
AbstractHash 4de0604c5643bf32440d0e380b7ca68e85f623a4de50fe33de2f355e
ghci> toLazyByteString $ lazyByteStringHex $ encode $ hash $ mkCoin 3
"4de0604c5643bf32440d0e380b7ca68e85f623a4de50fe33de2f355e"
~~~

### Public Key

~~~
-- | Wrapper around 'Ed25519.PublicKey'.
newtype PublicKey = PublicKey Ed25519.PublicKey
    deriving (Eq, Ord, Show, Generic, NFData, Hashable, Typeable)
~~~

| Field size | Type      | Description            |
|------------+-----------+------------------------|
|         32 | Word8[32] | 32 bytes of public key |

### Signature

~~~
-- | Wrapper around 'Ed25519.Signature'.
newtype Signature a = Signature Ed25519.Signature
    deriving (Eq, Ord, Show, Generic, NFData, Hashable, Typeable)
~~~

| Field size | Type      | Description                  |
|------------+-----------+------------------------------|
|         64 | Word8[64] | 64 bytes of signature string |

### Address

~~~
type AddressHash = Hash

-- | Stakeholder identifier (stakeholders are identified by their public keys)
type StakeholderId = AddressHash PublicKey

-- | Address is where you can send coins.
data Address
    = PubKeyAddress
          { addrKeyHash :: !(AddressHash PublicKey) }
    | ScriptAddress
          { addrScriptHash :: !(AddressHash Script) }
    deriving (Eq, Ord, Generic, Typeable)
~~~

| Field size | Type    | Description                                                                              |
| ---------- | ------- | -----------                                                                              |
|          1 | Word8   | Tag to select between constructors. 0x00 for `PubKeyAddress` and 0x01 for `ScriptAddress |
|         28 | Hash    | Hash for corresponding PublicKey

Example:

~~~
ghci> hash somePk
AbstractHash 7d9be76a0b384dbe8d408012b5f9a33978da79793a9602a65ed3a0f3
ghci> toLazyByteString $ lazyByteStringHex $ encode $ PubKeyAddress $ hash somePk
"007d9be76a0b384dbe8d408012b5f9a33978da79793a9602a65ed3a0f33103f2c5"
~~~

### Unsigned Variable length integer

This type will be referenced later as `UVarInt Word16` or `UVarInt Word64`
to describe maximum available value.

~~~
newtype UnsignedVarInt a = UnsignedVarInt {getUnsignedVarInt :: a}
    deriving (Eq, Ord, Show, Generic, NFData)
~~~

Values are encoded 7 bits at a time, with the most significant being a continuation bit.
Thus, the numbers from 0 to 127 require only a single byte to encode,
those from 128 to 16383 require two bytes, etc.

This format is taken from Google's Protocol Buffers, which provides a bit more verbiage on the encoding:
https://developers.google.com/protocol-buffers/docs/encoding#varints.

### Attributes

~~~
-- | Convenient wrapper for the datatype to represent it (in binary
-- format) as k-v map.
data Attributes () = Attributes
    { -- | Data, containing known keys (deserialized)
      attrData   :: ()
      -- | Unparsed ByteString
    , attrRemain :: ByteString
    }
  deriving (Eq, Ord, Generic, Typeable)
~~~

Stored as `totalLen + (k, v) pairs + some remaining part`. But currenlty there are
no `(key, value)` pairs in attributes, only arbitrary length byte array.

| Field size | Type     | Value | Description                                 |
|------------+----------+-------+---------------------------------------------|
| 4          | Word32   | n     | Size of attributes in bytes. Should <= 2^28 |
| n          | Word8[n] |       | `n` bytes of data                           |

Example: `Attributes () (BS.pack [1, 31]) --> 0x00000002011f`

### Script

~~~
-- | Version of script
type ScriptVersion = Word16

-- | A script for inclusion into a transaction.
data Script = Script {
    scrVersion :: ScriptVersion,    -- ^ Version
    scrScript  :: LByteString}      -- ^ Serialized script
  deriving (Eq, Show, Generic, Typeable)
~~~

| Field size | Type           | Value | Description        |
|------------+----------------+-------+--------------------|
|        1-3 | UVarInt Word16 |       | Script version     |
|        1-9 | UVarInt Int64  | n     | Size of byte array |
|          n | Word8[n]       |       | n bytes of script  |

### Lists and vectors

Sometimes we store list of some objects inside our datatypes. You will see references to them
as `Vector a` or `[a]`. You should read this as _array of objects of types `a`_. Both these
standard Haskell data types are serialized in the same way.

| Field size  | Type        | Value | Description                                  |
|-------------+-------------+-------+----------------------------------------------|
| 1-9         | UVarInt Int | n     | Size of array                                |
| n * size(a) | a[n]        |       | Array with length `n` of objects of type `a` |

## Headers

## Transaction sending

### Transaction signature data

~~~
-- | Represents transaction identifier as 'Hash' of 'Tx'.
type TxId = Hash Tx

-- | Data that is being signed when creating a TxSig.
type TxSigData = (TxId, Word32, Hash [TxOut], Hash TxDistribution)
~~~

| Field size | Type   | Description                         |
|------------+--------+-------------------------------------|
|         28 | Hash   | Signature of transaction            |
|          4 | Word32 | ???                                 |
|         28 | Hash   | Hash of list of transaction outputs |
|         28 | Hash   | Hash of transaction distribution    |

### Transaction witness

~~~
-- | 'Signature' of addrId.
type TxSig = Signature TxSigData

-- | A witness for a single input.
data TxInWitness
    = PkWitness { twKey :: PublicKey
                , twSig :: TxSig}
    | ScriptWitness { twValidator :: Script
                    , twRedeemer  :: Script}
    deriving (Eq, Show, Generic, Typeable)

-- | A witness is a proof that a transaction is allowed to spend the funds it
-- spends (by providing signatures, redeeming scripts, etc). A separate proof
-- is provided for each input.
type TxWitness = Vector TxInWitness
~~~

Table for `TxInWitness`:

| Tag size | Tag Type | Tag Value | Description           | Field size   | Field Type | Field name  |
|----------+----------+-----------+-----------------------+--------------+------------+-------------|
|        1 | Word8    |         0 | Tag for PkWitness     |              |            |             |
|          |          |           |                       | 32           | PublicKey  | twKey       |
|          |          |           |                       | 64           | TxSig      | twSig       |
|          |          |         1 | Tag for ScriptWitness |              |            |             |
|          |          |           |                       | size(Script) | Script     | twValidator |
|          |          |           |                       | size(Script) | Script     | twRedeemer  |

### Transaction input

~~~
-- | Transaction input.
data TxIn = TxIn
    { -- | Which transaction's output is used
      txInHash  :: !TxId
      -- | Index of the output in transaction's outputs
    , txInIndex :: !Word32
    } deriving (Eq, Ord, Show, Generic, Typeable)
~~~

| Field size | Type   | Field name   |
|------------+--------+--------------|
|         28 | Hash   | txOutAddress |
|          4 | Word32 | txOutValue   |

### Transaction output

~~~
-- | Transaction output.
data TxOut = TxOut
    { txOutAddress :: !Address
    , txOutValue   :: !Coin
    } deriving (Eq, Ord, Generic, Show, Typeable)
~~~

| Field size | Type      | Field name    |
| ---------- | --------- | ------------- |
|         29 | Address   | txOutAddress  |
|          8 | Coin      | txOutValue    |

Example:

`TxOut addr (mkCoin 31) --> 0x007d9be76a0b384dbe8d408012b5f9a33978da79793a9602a65ed3a0f33103f2c5000000000000001f`

~~~
-- | Transaction output auxilary data
type TxOutAux = (TxOut, [(StakeholderId, Coin)])
~~~

Lets define `distr_size(n) = n * (size(Hash) + size(Coin))`.

|    Field size | Type           | Value | Description                               |
|---------------+----------------+-------+-------------------------------------------|
|            37 | TxOut          |       | Transaction output                        |
|           1-9 | UVarInt Int    | n     | Length of list for output auxilary data   |
| distr_size(n) | <Hash,Coin>[n] |       | Array of pairs for StakeholderId and Coin |

### Transaction

~~~
-- | Transaction.
--
-- NB: transaction witnesses are stored separately.
data Tx = Tx
    { txInputs     :: ![TxIn]   -- ^ Inputs of transaction.
    , txOutputs    :: ![TxOut]  -- ^ Outputs of transaction.
    , txAttributes :: !TxAttributes -- ^ Attributes of transaction
    } deriving (Eq, Ord, Generic, Show, Typeable)
~~~

| Field size         | Type         | Value | Description                   |
|--------------------+--------------+-------+-------------------------------|
| 1-9                | UVarInt Int  | n     | Number of transaction inputs  |
| n * size(TxIn)     | TxIn[n]      |       | Array of transaction inputs   |
| 1-9                | UVarInt Int  | m     | Number of transaction outputs |
| m * size(TxOut)    | TxOut[m]     |       | Array of transaction outputs  |
| size(TxAttributes) | TxAttributes |       | Attributes for transaction    |

### Transaction distribution

~~~
-- | Distribution of “fake” stake that follow-the-satoshi would use for a
-- particular transaction.
newtype TxDistribution = TxDistribution {
    getTxDistribution :: [[(StakeholderId, Coin)]] }
    deriving (Eq, Show, Generic, Typeable)
~~~

Though transaction distribution can be stored as list of list using previous serialization
strategy it is often happens that we pass list of empty lists. In that case we store such
lists more efficiently.

| Tag size | Tag Type | Tag Value | Description              |    Field size | Field Type     | Value |
|----------+----------+-----------+--------------------------+---------------+----------------+-------|
|        1 | Word8    |         0 | List of empty lists      |               |                |       |
|          |          |           |                          |           1-9 | UVarInt Int    |       |
|          |          |         1 | Some lists are not empty |               |                |       |
|          |          |           |                          |           1-9 | UVarInt Int    | n     |
|          |          |           |                          | distr_size(n) | <Hash,Coin>[n] |       |

### Transaction auxilary

~~~
-- | Transaction + auxiliary data
type TxAux = (Tx, TxWitness, TxDistribution)
~~~

| Field size           | Type           | Description              |
|----------------------+----------------+--------------------------|
| size(Tx)             | Tx             | Transaction itself       |
| size(TxWitness)      | TxWitness      | Witness for transaction  |
| size(TxDistribution) | TxDistribution | Transaction distribution |
