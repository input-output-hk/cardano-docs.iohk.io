---
layout: default
title: Binary protocols
permalink: /protocols/binary-protocols/
group: protocols
---

# Binary protocols

Sizes of all fields are represented in bytes.
Big-Endian is used everywhere.

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

## Headers

## Transaction sending