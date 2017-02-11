---
layout: default
title: PureScript Type Bridge in Cardano SL
permalink: /cardano/pstypebridge/
group: cardano
---
[//]: # (Reviewed at e74b95fd7e04b43c03198dbed0f8599d53df5235)

# PureScript type bridge

In the [Wallets](/cardano/wallets/) section, it was discussed how the wallet
types are organized and how the API is structured. This section will be a
review of the way these types are used in the frontend, which was written in
PureScript.

## PureScript

While these documents are not meant to be an introduction to PureScript, it
should be known that it is a strict, strongly typed [programming language
](http://www.purescript.org/) that compiles directly to JavaScript. It is
very similar to Haskell in many regards, including its terse, expressive
and safe code, which makes it a good candidate for frontend work if functional
languages are preferred.

## Type Bridging

Since all of Cardano's wallet was written in Haskell, to interact with the
frontend and send/receive information to/from it is necessary to translate
these Haskell types in PureScript types, and so on. Because the two languages
are similar, this can be done via the `purescript-bridge` package, which
exports, among other important functions and types we'll use, the `mkSumType`
function. This function works for any Haskell type that has an equivalent in
PureScript, like Haskell's `Int` and PureScript's `Number`, Haskell's strings
and PureScript's `String`, Haskell's tuple, `(,)`, and PureScript's
`Tuple`, Haskell's records, and PureScript's, which only have a slight
difference in syntax, and so on.

In these cases, all that one needs to write is, for example,

~~~ haskell
import         Language.PureScript.Bridge (mkSumType, buildBridge,
										   defaultBridge, writePSTypes)

main :: IO ()
main =
	writePSTypes
	(buildBridge defaultBridge)
	  "PureScript/Type/Directory"
	  [ mkSumType (Proxy @MyType)
	  ]
~~~

where `writePSTypes` is a function from `purescript-bridge` that will write the
generated PureScript types to the specified directory, mirroring the hierarchy
of the Haskell modules the types came from. The `buildBridge` line will be
important for the next section.

This is what was done for most types in the wallet's backend. For the rest,
another mechanism was required.

## Custom bridges

Like mentioned before, `mkSumType` works for most types, but for Haskell's
`Word`, in this project's case, it did not. This is because PureScript does not
have the variety of numerical types that Haskell does, so for `Word, Word8,
Word 32, Word64` and other very specific integral types, PureScript's compiler
is unable to automatically perform the conversion. In these cases, we require a
custom bridge. Such a mechanism is simple. Building on the previous example:

~~~ haskell
import           Language.PureScript.Bridge         (BridgePart, buildBridge,
													 defaultBridge, mkSumType, typeName,
													 writePSTypes, (<|>), (^==))
import           Language.PureScript.Bridge.PSTypes (psInt)

main :: IO ()
main =
	writePSTypes
	  "PureScript/Type/Directory"
	  (buildBridge customBridge)
	  [ mkSumType (Proxy @MyType)
	  ]
  where
	customBridge =
		defaultBridge <|> wordBridge

wordBridge :: BridgePart
wordBridge = typeName ^== "Word" >> pure psInt
~~~

This means a type called `Word` will be created in the generated PureScript
files, which will be a newtype wrapper ove PureScript's integer type.

For all types for which a custom bridge is needed, after writing one, the bridge
should simply be appended to the `customBridge` being used, with the `(<|>)`
combinator.
