# hw-rankselect
[![CircleCI](https://circleci.com/gh/haskell-works/hw-rankselect.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-rankselect)

Efficient `rank` and `select` operations on large bit-vectors.

This library will use support for some BMI2 CPU instructions on some x86 based
CPUs if compiled with the appropriate flags on `ghc-8.4.1` or later.

## Rank and select

This library provides the following functions on indexed bit-vectors:

* `rank1`
* `rank0`
* `select1`
* `select0`

The supported indexed bit-vector types are:

* `Poppy512`
* `CsPoppy`

## Constructing and using an indexed bit-vector in the repl

The example below constructs an indexed bit-vector from a string and runs
rank and select query operations on it.  The bits in a string are in
little-endian and can be of arbitrary length.  The resulting bit-vector
will be padded with 0-bits until the next 64-bit boundary.

```text
$ stack repl  --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2
λ> import HaskellWorks.Data.Bits.BitRead
λ> let bs = fromJust $ bitRead "10010010" :: CsPoppy
bs :: CsPoppy
λ> select1 bs 1
1
λ> select1 bs 2
4
λ> select1 bs 3
7
λ> rank1 bs 7
3
λ> rank1 bs 4
2
λ> rank1 bs 1
1
```

A valid bit string contains zero or more characters.  Characters other than `1` and `0` are
permitted, but are ignored.  For example spaces can be used to group bits for clarity.

```text
λ> let bs = fromJust $ bitRead "" :: CsPoppy
bs :: CsPoppy
λ> let bs = fromJust $ bitRead "10010010 10010010" :: CsPoppy
bs :: CsPoppy
```

Whilst the use of a bit string is convenient for the repl, for performance reasons, it
is more typical to construct an indexed bit-vector from a 64-bit word vector:

```text
> import qualified Data.Vector.Storable as DVS
λ> let bs = makeCsPoppy (DVS.fromList [23, 73, 55])
bs :: CsPoppy
```

## Compilation

It is sufficient to build, test and benchmark the library as follows
for basic performance.  The library will be compiled to use broadword
implementation of rank & select, which has reasonable performance.

```text
stack build
stack test
stack bench
```

To target the BMI2 instruction set, add the `bmi2` flag:

```text
stack build --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2
stack test  --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2
stack bench --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2
```

## Benchmark results

The following benchmark shows the kinds of performance gain that can
be expected from enabling the BMI2 instruction set for CPU targets
that support them:

```text
benchmarking data/48-mb-bitvector/CsPoppy Select1
time                 3.636 μs   (3.613 μs .. 3.669 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 3.618 μs   (3.607 μs .. 3.636 μs)
std dev              46.03 ns   (30.79 ns .. 78.23 ns)
```

```text
benchmarking data/48-mb-bitvector/CsPoppy Select1
time                 1.969 μs   (1.959 μs .. 1.982 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.973 μs   (1.963 μs .. 1.986 μs)
std dev              38.41 ns   (26.87 ns .. 59.08 ns)
variance introduced by outliers: 21% (moderately inflated)```
```

## References

* [Space-Efficient, High-Performance Rank & Select Structures on Uncompressed Bit Sequences](http://www.cs.cmu.edu/~./dga/papers/zhou-sea2013.pdf)
