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

`Word32` is _unsigned integer of 32 bits_ (`uint32`).

To test serialization of objet `myObject` in `ghci`,
one should use the following command (which will be referenced later as just `hexEncode myObject`):

~~~
ghci> toLazyByteString $ lazyByteStringHex $ encode $ myObject
~~~

## Common Haskell data types

### Either

~~~ haskell
data Either a b = Left a | Right b
~~~

`Either a b` is either value of type `a` or value of type `b`. To distinguish between
two values we add 1 byte tag before data.

| Tag size | Tag Type | Tag Value | Description   | Field size | Description       |
|----------+----------+-----------+---------------+------------+-------------------|
|        1 | Word8    |      0x00 | Tag for Left  |            |                   |
|          |          |           |               | size(a)    | Value of type `a` |
|          |          |      0x01 | Tag for Right |            |                   |
|          |          |           |               | size(b)    | Value of type `b` |

Example:

~~~
ghci> hexEncode (Left  3 :: Either Word16 Word32)
"000003"
ghci> hexEncode (Right 4 :: Either Word16 Word32)
"0100000004"
~~~

### Lists and vectors

Sometimes we store list of some objects inside our datatypes. You will see references to them
as `Vector a` or `[a]`. You should read this as _array of objects of types `a`_. Both these
standard Haskell data types are serialized in the same way.

| Field size  | Type        | Value | Description                                  |
|-------------|-------------|-------|----------------------------------------------|
| 1-9         | UVarInt Int | n     | Size of array                                |
| n * size(a) | a[n]        |       | Array with length `n` of objects of type `a` |

Example:

~~~
ghci> hexEncode ([1, 31] :: [Word16])
"020001001f"
ghci> hexEncode ([0..135] :: [Word8])  -- 136 bytes from 0 to 135 including
"8801000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f2021222324252
62728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4
f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f70717273747576777
8797a7b7c7d7e7f8081828384858687"
~~~

## Basic Cardano-SL data types

### Coin

~~~ haskell
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

~~~ haskell
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
ghci> hexEncode $ hash $ mkCoin 3
"4de0604c5643bf32440d0e380b7ca68e85f623a4de50fe33de2f355e"
~~~

### Public Key

~~~ haskell
-- | Wrapper around 'Ed25519.PublicKey'.
newtype PublicKey = PublicKey Ed25519.PublicKey
    deriving (Eq, Ord, Show, Generic, NFData, Hashable, Typeable)
~~~

| Field size | Type      | Description            |
|------------|-----------|------------------------|
|         32 | Word8[32] | 32 bytes of public key |

### Signature

~~~ haskell
-- | Wrapper around 'Ed25519.Signature'.
newtype Signature a = Signature Ed25519.Signature
    deriving (Eq, Ord, Show, Generic, NFData, Hashable, Typeable)
~~~

| Field size | Type      | Description                  |
|------------|-----------|------------------------------|
|         64 | Word8[64] | 64 bytes of signature string |

### SlotId

~~~ haskell
-- | Index of epoch.
newtype EpochIndex = EpochIndex
    { getEpochIndex :: Word64
    } deriving (Show, Eq, Ord, Num, Enum, Integral, Real, Generic, Hashable, Bounded, Typeable)

-- | Index of slot inside a concrete epoch.
newtype LocalSlotIndex = LocalSlotIndex
    { getSlotIndex :: Word16
    } deriving (Show, Eq, Ord, Num, Enum, Ix, Integral, Real, Generic, Hashable, Buildable, Typeable)

-- | Slot is identified by index of epoch and local index of slot in
-- this epoch. This is a global index
data SlotId = SlotId
    { siEpoch :: !EpochIndex
    , siSlot  :: !LocalSlotIndex
    } deriving (Show, Eq, Ord, Generic, Typeable)
~~~

| Field size | Type    | Description                        |
| ---------- | ------- | ---------------------------------- |
|          8 | uint64  | Epoch index                        |
|          2 | uint16  | Slot index inside a concrete epoch |

Example:

[//]: TODO: Add example

### Address

~~~ haskell
type AddressHash = Hash

-- | Stakeholder identifier (stakeholders are identified by their public keys)
type StakeholderId = AddressHash PublicKey

-- | Address is where you can send coins.
data Address
    = PubKeyAddress
          { addrKeyHash :: !StakeholderId }
    | ScriptAddress
          { addrScriptHash :: !(AddressHash Script) }
    deriving (Eq, Ord, Generic, Typeable)
~~~

| Field size | Type    | Description                                                                               |
| ---------- | ------- | -----------                                                                               |
|          1 | Word8   | Tag to select between constructors. 0x00 for `PubKeyAddress` and 0x01 for `ScriptAddress` |
|         28 | Hash    | Hash for corresponding PublicKey

Example:

~~~
ghci> hash somePk
AbstractHash 7d9be76a0b384dbe8d408012b5f9a33978da79793a9602a65ed3a0f3
ghci> hexEncode $ PubKeyAddress $ hash somePk
"007d9be76a0b384dbe8d408012b5f9a33978da79793a9602a65ed3a0f33103f2c5"
~~~

### Unsigned Variable length integer

This type will be referenced later as `UVarInt Word16` or `UVarInt Word64`
to describe maximum available value.

~~~ haskell
newtype UnsignedVarInt a = UnsignedVarInt {getUnsignedVarInt :: a}
    deriving (Eq, Ord, Show, Generic, NFData)
~~~

Values are encoded 7 bits at a time, with the most significant being a continuation bit.
Thus, the numbers from 0 to 127 require only a single byte to encode,
those from 128 to 16383 require two bytes, etc.

This format is taken from Google's Protocol Buffers, which provides a bit more verbiage on the encoding:
https://developers.google.com/protocol-buffers/docs/encoding#varints.

### Attributes

~~~ haskell
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
no `(key, value)` pairs in attributes, only arbitrary length byte array:

| Field size | Type     | Value | Description                                 |
|------------|----------|-------|---------------------------------------------|
| 4          | Word32   | n     | Size of attributes in bytes. Should <= 2^28 |
| n          | Word8[n] |       | `n` bytes of data                           |

Example: `Attributes () (BS.pack [1, 31]) --> 0x00000002011f`

### Script

~~~ haskell
-- | Version of script
type ScriptVersion = Word16

-- | A script for inclusion into a transaction.
data Script = Script {
    scrVersion :: ScriptVersion,    -- ^ Version
    scrScript  :: LByteString}      -- ^ Serialized script
  deriving (Eq, Show, Generic, Typeable)
~~~

| Field size | Type           | Value | Description        |
|------------|----------------|-------|--------------------|
|        1-3 | UVarInt Word16 |       | Script version     |
|        1-9 | UVarInt Int64  | n     | Size of byte array |
|          n | Word8[n]       |       | n bytes of script  |


### Raw

~~~ haskell
-- | A wrapper over 'ByteString' for adding type safety to
-- 'Pos.Crypto.Pki.encryptRaw' and friends.
newtype Raw = Raw ByteString
    deriving (Bi, Eq, Ord, Show, Typeable)
~~~

| Field size | Type        | Value | Description    |
|------------|-------------|-------|----------------|
| 1-9        | UVarInt Int | n     | Length of data |
| n          | Word8[n]    |       | Data           |

### MerkleRoot

~~~ haskell
-- | Data type for root of merkle tree.
newtype MerkleRoot a = MerkleRoot
    { getMerkleRoot :: Hash Raw  -- ^ returns root 'Hash' of Merkle Tree
    } deriving (Show, Eq, Ord, Generic, ByteArrayAccess, Typeable)
~~~

| Field size | Type     | Description              |
|------------|----------|--------------------------|
|         28 | Hash Raw | Root hash of Merkle tree |


## Headers

### BlockVersion

~~~ haskell
-- | Communication protocol version.
data BlockVersion = BlockVersion
    { bvMajor :: !Word16
    , bvMinor :: !Word16
    , bvAlt   :: !Word8
    } deriving (Eq, Generic, Ord, Typeable)
~~~

| Field size | Type   | Description        |
|------------|--------|--------------------|
|          2 | Word16 | Major version      |
|          2 | Word16 | Minor version      |
|          1 | Word8  | TODO: what is Alt? |

### SoftwareVersion

~~~ haskell
newtype ApplicationName = ApplicationName
    { getApplicationName :: Text
    } deriving (Eq, Ord, Show, Generic, Typeable, ToString, Hashable, Buildable, ToJSON, FromJSON)

-- | Software version.
data SoftwareVersion = SoftwareVersion
    { svAppName :: !ApplicationName
    , svNumber  :: !NumSoftwareVersion
    } deriving (Eq, Generic, Ord, Typeable)
~~~

| Field size | Type        | Value | Description                   |
|------------|-------------|-------|-------------------------------|
| 1-9        | UVarInt Int | n     | Length of application name    |
| n          | Word8[n]    |       | UTF8 encoded application name |
|            |             |       |                               |

### HeaderHash

~~~ haskell
-- | 'Hash' of block header. This should be @Hash (BlockHeader ssc)@
-- but we don't want to have @ssc@ in 'HeaderHash' type.
type HeaderHash = Hash BlockHeaderStub
data BlockHeaderStub
~~~

### GodTossing

#### GtProof

~~~ haskell
-- | Proof of MpcData.
-- We can use ADS for commitments, openings, shares as well,
-- if we find it necessary.
data GtProof
    = CommitmentsProof !(Hash CommitmentsMap) !(Hash VssCertificatesMap)
    | OpeningsProof !(Hash OpeningsMap) !(Hash VssCertificatesMap)
    | SharesProof !(Hash SharesMap) !(Hash VssCertificatesMap)
    | CertificatesProof !(Hash VssCertificatesMap)
    deriving (Show, Eq, Generic)
~~~

[//]: TODO: Add descriptions and code for maps to the basic types section

| Tag size | Tag Type | Tag Value | Description               | Field size | Field Type              |
|----------|----------|-----------|---------------------------|------------|-------------------------|
|        1 | Word8    |         0 | Tag for CommitmentsProof  |            |                         |
|          |          |           |                           |         28 | Hash CommitmentsMap     |
|          |          |           |                           |         28 | Hash VssCertificatesMap |
|          |          |         1 | Tag for OpeningsProof     |            |                         |
|          |          |           |                           |         28 | Hash OpeningsMap        |
|          |          |           |                           |         28 | Hash VssCertificatesMap |
|          |          |         2 | Tag for SharesProof       |            |                         |
|          |          |           |                           |         28 | Hash SharesMap          |
|          |          |           |                           |         28 | Hash VssCertificatesMap |
|          |          |         3 | Tag for CertificatesProof |            |                         |
|          |          |           |                           |         28 | Hash VssCertificatesMap |

### MainBlockHeader

[//]: TODO: Replace all Main* and Genesis* by type (*Blockchain)
[//]: TODO: Add code for MainBlockHeader

| Field size                | Type                | Description         |
| ----------                | -------             | -----------         |
| 4                         | Word32              | Protocol magic      |
| 28                        | HeaderHash          | Previous block hash |
| size(MainProof)           | MainProof           | Body proof          |
| size(MainConsensusData)   | MainConsensusData   | Consensus data      |
| size(MainExtraHeaderData) | MainExtraHeaderData | MainExtraHeaderData |

#### MainProof

[//]: TODO: Add code for MainProof

|       Field size | Type                 | Description     |
|       ---------- | -------              | -----------     |
|                4 | Word32               | mpNumber        |
| size(MerkleRoot) | MerkleRoot Tx        | mpRoot          |
|               28 | Hash [TxWitness]     | mpWitnessesHash |
|    size(GtProof) | GtProof              | mpMpcProof      |
|               28 | Hash [ProxySKSimple] | mpProxySKsProof |
|               28 | UpdateProof          | mpUpdateProof   |

#### MainConsensusData

~~~ haskell
-- | Chain difficulty represents necessary effort to generate a
-- chain. In the simplest case it can be number of blocks in chain.
newtype ChainDifficulty = ChainDifficulty
    { getChainDifficulty :: Word64
    } deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Generic, Buildable, Typeable)
~~~

| Field size | Type             | Description   |
| ---------- | ---------------- | ------------- |
| 10         | SlotId           | mcdSlot       |
| 32         | PublicKey        | mcdLeaderKey  |
| 8          | ChainDifficulty  | mcdDifficulty |
| 64         | BlockSignature   | mcdSignature  |

#### MainExtraHeaderData

~~~ haskell
-- | Represents main block header extra data
data MainExtraHeaderData = MainExtraHeaderData
    { -- | Version of block.
      _mehBlockVersion    :: !BlockVersion
    , -- | Software version.
      _mehSoftwareVersion :: !SoftwareVersion
    , -- | Header attributes
      _mehAttributes      :: !BlockHeaderAttributes
    }
    deriving (Eq, Show, Generic)

-- | Represents main block header attributes: map from 1-byte integer to
-- arbitrary-type value. To be used for extending header with new
-- fields via softfork.
type BlockHeaderAttributes = Attributes ()
~~~

| Field size                  | Type                  | Description                                                                |
|-----------------------------|-----------------------|----------------------------------------------------------------------------|
| size(BlockVersion)          | BlockVersion          | Version of block                                                           |
| size(SoftwareVersion)       | SoftwareVersion       | Software version                                                           |
| size(BlockHeaderAttributes) | BlockHeaderAttributes | Header attributes (used for extending header with new fields via softfork) |

### GenesisBlockHeader

[//]: TODO: Add code for GenesisBlockHeader

| Field size                 | Type                 | Description         |
| ----------                 | -------              | -----------         |
| 4                          | Word32               | Protocol magic      |
| 28                         | HeaderHash           | Previous block hash |
| size(GenesisProof)         | GenesisProof         | Body proof          |
| size(GenesisConsensusData) | GenesisConsensusData | Consensus data      |

#### GenesisProof

[//]: TODO: SlotLeaders type

| Field size | Type             | Description               |
|------------|------------------|---------------------------|
|         28 | Hash SlotLeaders | Hash of slot leaders list |

#### GenesisConsensusData

| Field | Type            | Description                                             |
|-------|-----------------|---------------------------------------------------------|
|     8 | EpochIndex      | Index of epoch for which this genesis block is relevant |
|     8 | ChainDifficulty | Difficulty of the chain ending in this genesis block.   |

## Transaction sending

To send transaction you need to create and send `TxAux` data type to node. All data types
required to successfully perform sending are described in this section.

### Transaction input

~~~ haskell
-- | Represents transaction identifier as 'Hash' of 'Tx'.
type TxId = Hash Tx

-- | Transaction input.
data TxIn = TxIn
    { -- | Which transaction's output is used
      txInHash  :: !TxId
      -- | Index of the output in transaction's outputs
    , txInIndex :: !Word32
    } deriving (Eq, Ord, Show, Generic, Typeable)
~~~

| Field size | Type   | Field name   |
|------------|--------|--------------|
|         28 | Hash   | txOutAddress |
|          4 | Word32 | txOutValue   |

### Transaction output

~~~ haskell
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

~~~
ghci> hexEncode $ TxOut addr (mkCoin 31)
0x007d9be76a0b384dbe8d408012b5f9a33978da79793a9602a65ed3a0f33103f2c5000000000000001f`
~~~

### Transaction output auxilary

~~~ haskell
-- | Transaction output auxilary data
type TxOutAux = (TxOut, [(StakeholderId, Coin)])
~~~

Lets define `distr_size(n) = n * (size(Hash) + size(Coin))`.

|    Field size | Type           | Value | Description                               |
|---------------|----------------|-------|-------------------------------------------|
|            37 | TxOut          |       | Transaction output                        |
|           1-9 | UVarInt Int    | n     | Length of list for output auxilary data   |
| distr_size(n) | <Hash,Coin>[n] |       | Array of pairs for StakeholderId and Coin |

### Transaction signature data

~~~ haskell

-- | Data that is being signed when creating a TxSig.
type TxSigData = (TxId, Word32, Hash [TxOut], Hash TxDistribution)
~~~

| Field size | Type   | Description                                                              |
|------------+--------+--------------------------------------------------------------------------|
|         28 | Hash   | Signature of transaction                                                 |
|          4 | Word32 | Index of the output in transaction's outputs (see `txInIndex` in `TxIn`) |
|         28 | Hash   | Hash of list of transaction outputs                                      |
|         28 | Hash   | Hash of transaction distribution                                         |

### Transaction witness

~~~ haskell
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
|        1 | Word8    |      0x00 | Tag for PkWitness     |              |            |             |
|          |          |           |                       | 32           | PublicKey  | twKey       |
|          |          |           |                       | 64           | TxSig      | twSig       |
|          |          |      0x01 | Tag for ScriptWitness |              |            |             |
|          |          |           |                       | size(Script) | Script     | twValidator |
|          |          |           |                       | size(Script) | Script     | twRedeemer  |


### Transaction

~~~ haskell
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
|--------------------|--------------|-------|-------------------------------|
| 1-9                | UVarInt Int  | n     | Number of transaction inputs  |
| n * size(TxIn)     | TxIn[n]      |       | Array of transaction inputs   |
| 1-9                | UVarInt Int  | m     | Number of transaction outputs |
| m * size(TxOut)    | TxOut[m]     |       | Array of transaction outputs  |
| size(TxAttributes) | TxAttributes |       | Attributes for transaction    |

### Transaction distribution

~~~ haskell
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
|        1 | Word8    |      0x00 | List of empty lists      |               |                |       |
|          |          |           |                          |           1-9 | UVarInt Int    |       |
|          |          |      0x01 | Some lists are not empty |               |                |       |
|          |          |           |                          |           1-9 | UVarInt Int    | n     |
|          |          |           |                          | distr_size(n) | <Hash,Coin>[n] |       |

### Transaction auxilary

~~~ haskell
-- | Transaction + auxiliary data
type TxAux = (Tx, TxWitness, TxDistribution)
~~~

| Field size           | Type           | Description              |
|----------------------|----------------|--------------------------|
| size(Tx)             | Tx             | Transaction itself       |
| size(TxWitness)      | TxWitness      | Witness for transaction  |
| size(TxDistribution) | TxDistribution | Transaction distribution |

[//]: TODO: describe full creation of transaction by bytes

## Delegation

Description of *Delegation* process in CSL-application chapter. Here is only
description of message formats.

### Proxy certificate

Similar to `Signature`.

~~~ haskell
-- | Proxy certificate, made of ω + public key of delegate.
newtype ProxyCert w = ProxyCert { unProxyCert :: Ed25519.Signature }
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)
~~~

| Field size | Type      | Description                                 |
|------------+-----------+---------------------------------------------|
|         64 | Word8[64] | `unProxyCert`: 64 bytes of signature string |

### Proxy secret key

~~~ haskell
-- | Convenient wrapper for secret key, that's basically ω + certificate.
data ProxySecretKey w = ProxySecretKey
    { pskOmega      :: w
    , pskIssuerPk   :: PublicKey
    , pskDelegatePk :: PublicKey
    , pskCert       :: ProxyCert w
    } deriving (Eq, Ord, Show, Generic)
~~~

| Field size | Type        | Description   |
|------------+-------------+---------------|
|    size(w) | w           | pskOmega      |
|         32 | PublicKey   | pskIssuerPk   |
|         32 | PublicKey   | pskDelegatePk |
|         64 | ProxyCert w | pskCert       |

### Proxy signature

~~~ haskell
-- | Delegate signature made with certificate-based permission. @a@
-- stays for message type used in proxy (ω in the implementation
-- notes).
data ProxySignature w a = ProxySignature
    { pdOmega      :: w
    , pdDelegatePk :: PublicKey
    , pdCert       :: ProxyCert w
    , pdSig        :: Ed25519.Signature
    } deriving (Eq, Ord, Show, Generic)
~~~

| Field size | Type        | Description  |
|------------+-------------+--------------|
|    size(w) | w           | pdOmega      |
|         32 | PublicKey   | pdDelegatePk |
|         64 | ProxyCert w | pdCert       |
|         64 | Signature   | pdSig        |

### Proxy secret key for lightweight delegation

~~~ haskell
-- | Correspondent SK for no-ttl proxy signature scheme.
type ProxySKSimple = ProxySecretKey ()
~~~

| Field size | Type         | Description   |
|------------+--------------+---------------|
|         32 | PublicKey    | pskIssuerPk   |
|         32 | PublicKey    | pskDelegatePk |
|         64 | ProxyCert () | pskCert       |

Note that we skip *pskOmega* field in the table above, because it has type *()* and is serialized into empty string.

### Proxy secret key for heavyweight delegation


#### Signature

~~~ haskell
-- | Proxy signature used in csl -- holds a pair of epoch
-- indices. Block is valid if it's epoch index is inside this range.
type ProxySigEpoch a = ProxySignature (EpochIndex, EpochIndex) a
~~~

| Field size | Type                               | Description  |
|------------+------------------------------------+--------------|
|       1-10 | UVarInt Word64                     | from epoch   |
|       1-10 | UVarInt Word64                     | to epoch     |
|         32 | PublicKey                          | pdDelegatePk |
|         64 | ProxyCert (EpochIndex, EpochIndex) | pdCert       |
|         64 | Signature                          | pdSig        |

#### Secret key

~~~ haskell
-- | Same alias for the proxy secret key (see 'ProxySigEpoch').
type ProxySKEpoch = ProxySecretKey (EpochIndex, EpochIndex)
~~~

| Field size | Type                               | Description   |
|------------+------------------------------------+---------------|
|       1-10 | UVarInt Word64                     | from epoch    |
|       1-10 | UVarInt Word64                     | to epoch      |
|         32 | PublicKey                          | pskIssuerPk   |
|         32 | PublicKey                          | pskDelegatePk |
|         64 | ProxyCert (EpochIndex, EpochIndex) | pskCert       |

### Proxy delegation start message

~~~ haskell
-- | Message with delegated proxy secret key. Is used to propagate
-- both epoch-oriented psks (lightweight) and simple (heavyweight).
data SendProxySK
    = SendProxySKEpoch !ProxySKEpoch
    | SendProxySKSimple !ProxySKSimple
    deriving (Show, Eq, Generic)
~~~

| Tag size | Tag Type | Tag Value | Description               | Field size          | Description |
|----------+----------+-----------+---------------------------+---------------------+-------------|
|        1 | Word8    |      0x00 | Tag for SendProxySKEpoch  |                     |             |
|          |          |           |                           | size(ProxySKEpoch)  | heavyweight |
|          |          |      0x01 | Tag for SendProxySKSimple |                     |             |
|          |          |           |                           | size(ProxySKSimple) | lightweigh  |

### Lighweight delegation confirmation

#### ConfirmProxySK

~~~ haskell
-- | Confirmation of proxy signature delivery. Delegate should take
-- the proxy signing key he has and sign this key with itself. If the
-- signature is correct, then it was done by delegate (guaranteed by
-- PSK scheme). Checking @w@ can be done with @(const True)@
-- predicate, because certificate may be sent in epoch id that's
-- before lower cert's @EpochIndex@.
data ConfirmProxySK =
    ConfirmProxySK !ProxySKEpoch !(ProxySigEpoch ProxySKEpoch)
    deriving (Show, Eq, Generic)
~~~

| Field size          | Description           |
|---------------------+-----------------------|
| size(ProxySKEpoch)  | certificate           |
| size(ProxySigEpoch) | proof for certificate |

#### CheckProxySKConfirmed

~~~ haskell
-- | Request to check if a node has any info about PSK delivery.
data CheckProxySKConfirmed =
    CheckProxySKConfirmed !ProxySKEpoch
    deriving (Show, Eq, Generic)
~~~

| Field size         | Description |
|--------------------+-------------|
| size(ProxySKEpoch) | certificate |

#### CheckProxySKConfirmedRes

~~~ haskell
-- | Response to the @CheckProxySKConfirmed@ call.
data CheckProxySKConfirmedRes =
    CheckProxySKConfirmedRes !Bool
    deriving (Show, Eq, Generic)
~~~

| Value size | Value Type | Value | Result |
|------------+------------+-------+--------|
|          1 | Word8      |  0x00 | False  |
|            |            |  0x01 | True   |
