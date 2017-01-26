---
layout: default
title: Binary protocols
permalink: /protocols/binary-protocols/
group: protocols
---

# Binary protocols

Sizes of all fields are represented in bytes.
Big-Endian is used everywhere. If type of some field is not primitive
(like unsigned 64bit int) -- our type -- than full body of this data type
is just concatenated and added after previously described field.

> Developer notes:
> To test in `ghci` you should use next command:
> ```
> ghci> toLazyByteString $ lazyByteStringHex $ encode $ myObject
> ```

## Common datatypes

### Coin

```
-- | Coin is the least possible unit of currency.
newtype Coin = Coin
    { getCoin :: Word64
    } deriving (Show, Ord, Eq, Bounded, Generic, Hashable, Data, NFData)
```

| Field size | Type   | Field name |
| ---------- |--------| ---------- |
| 8          | uint64 | getCoin    |

Example: `Coin 31 --> 0x000000000000001F`

### Hash

```
-- | Hash wrapper with phantom type for more type-safety.
-- Made abstract in order to support different algorithms in
-- different situations
newtype AbstractHash algo a = AbstractHash (Digest algo)
    deriving (Show, Eq, Ord, ByteArray.ByteArrayAccess, Generic, NFData)

-- | Type alias for commonly used hash
type Hash = AbstractHash Blake2s_224
```

| Field size | Type      | Description             |
| ---------- |-----------| ----------------------- |
| 28         | fixed_hex | 224 bits of hash digest |

Example:

```
ghci> hash $ mkCoin 3
AbstractHash 4de0604c5643bf32440d0e380b7ca68e85f623a4de50fe33de2f355e
ghci> toLazyByteString $ lazyByteStringHex $ encode $ hash $ mkCoin 3
"4de0604c5643bf32440d0e380b7ca68e85f623a4de50fe33de2f355e"
```

### MerkleTree

### SlotId

```
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
```

| Field size | Type    | Description                        |
| ---------- | ------- | ---------------------------------- |
|          8 | uint64  | Epoch index                        |
|          2 | uint16  | Slot index inside a concrete epoch |

Example:
[//]: TODO: Add example

### PublicKey

| Field size | Type       | Description                           |
| ---------- | -------    | ----------------------------------    |
| 32         | ByteString | Public key as bytestring of length 32 |

Example:
[//]: TODO: Add example

### Address

```
type AddressHash = Hash

-- | Address is where you can send coins.
data Address
    = PubKeyAddress
          { addrKeyHash :: !(AddressHash PublicKey) }
    | ScriptAddress
          { addrScriptHash :: !(AddressHash Script) }
    deriving (Eq, Ord, Generic, Typeable)
```

| Field size | Type  | Description |
| ---------- |-------| ----------- |
| 1          | uint8 | Tag to select between constructors. 0x00 for `PubKeyAddress` and 0x01 for `ScriptAddress |
| 28         | Hash  | Hash for corresponding data |

Example:

```
ghci> hash somePk
AbstractHash 7d9be76a0b384dbe8d408012b5f9a33978da79793a9602a65ed3a0f3
ghci> toLazyByteString $ lazyByteStringHex $ encode $ PubKeyAddress $ hash somePk
"007d9be76a0b384dbe8d408012b5f9a33978da79793a9602a65ed3a0f33103f2c5"
```

## Headers

### HeaderHash

```
-- | 'Hash' of block header. This should be @Hash (BlockHeader ssc)@
-- but we don't want to have @ssc@ in 'HeaderHash' type.
type HeaderHash = Hash BlockHeaderStub
data BlockHeaderStub
```

### MainProof

| Field size | Type                 | Description     |
| ---------- | -------              | -----------     |
| 4          | Word32               | mpNumber        |
| ?          | MerkleRoot Tx        | mpRoot          |
| 28         | Hash [TxWitness]     | mpWitnessesHash |
| ?          | SscProof ssc         | mpMpcProof      |
| ?          | Hash [ProxySKSimple] | mpProxySKsProof |
| 28         | UpdateProof          | mpUpdateProof   |

### MainConsensusData


| Field size | Type             | Description   |
| ---------- | ---------------- | ------------- |
| ?          | SlotId           | mcdSlot       |
| ?          | PublicKey        | mcdLeaderKey  |
| ?          | ChainDifficulty  | mcdDifficulty |
| ?          | BlockSignature   | mcdSignature  |

### GenericBlockHeader

| Field size | Type            | Description         |
| ---------- | -------         | -----------         |
| 4          | uint32????      | Protocol magic      |
| 28         | HeaderHash      | Previous block hash |
| ?          | MainProof       | Body proof          |
| ?          | ConsensusData   | gbhConsensus        |
| ?          | ExtraHeaderData | ExtraHeaderData     |


## Transaction sending

### Transaction output

```
-- | Transaction output.
data TxOut = TxOut
    { txOutAddress :: !Address
    , txOutValue   :: !Coin
    } deriving (Eq, Ord, Generic, Show, Typeable)
```

| Field size | Type    |
| ---------- |---------|
| 29         | Address |
| 8          | Coin    |

Example:

`TxOut addr (mkCoin 31) --> 0x007d9be76a0b384dbe8d408012b5f9a33978da79793a9602a65ed3a0f33103f2c5000000000000001f`
