[![Build Status](https://travis-ci.org/arranstewart/hs-typed-digits.svg?branch=master)](https://travis-ci.org/arranstewart/hs-typed-digits)
[![Hackage](https://img.shields.io/hackage/v/typed-digits.svg)](https://hackage.haskell.org/package/typed-digits)

# typed-digits

Provides a Digit type, with the base of the digit available
as a type-level Nat. Thus, it can be guaranteed at compile-time
that digits of different bases can't be added together.

## Usage example

For more convenient provision of type-level parameters,
it's recommended to use the `DataKinds` and `TypeApplications` extensions.
Then, you can give type-level parameters using `@`, as seen below, instead of
giving the full type.

```haskell
>>> :set -XDataKinds -XTypeApplications
>>> import Data.TypedDigits
>>> let d = digit @9 3
>>> d
Just 3 (base 9)
```

## Source tree contents 

-   `src`: Library source files
-   `test`: Unit tests
-   `doctest`: Documentation tests using [`doctest`](https://github.com/sol/doctest)


