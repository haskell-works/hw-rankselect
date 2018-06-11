# hw-rankselect
[![CircleCI](https://circleci.com/gh/haskell-works/hw-rankselect.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-rankselect)
[![Travis](https://travis-ci.org/haskell-works/hw-rankselect.svg?branch=master)](https://travis-ci.org/haskell-works/hw-rankselect)

Efficient `rank` and `select` operations on large bit-vectors based on the paper
"Space-Efficient, High-Performance Rank & Select Structures on Uncompressed Bit
Sequences".  This library does not yet implement the full cspoppy implementation.
Notably, it still uses the sub-optimal "Straw man" design for "Combined Sampling"
on page 10.

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
$ stack repl --package hw-rankselect --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2 --flag hw-rankselect:bmi2
λ> import Data.Maybe
λ> import HaskellWorks.Data.Bits.BitRead
λ> import HaskellWorks.Data.RankSelect.Base.Rank1
λ> import HaskellWorks.Data.RankSelect.Base.Select1
λ> import HaskellWorks.Data.RankSelect.CsPoppy
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

## Working with files

Bit strings are stored in files as a string of bits (little-endian, which is native for
Intel platforms) padded to the nearest word8 (byte) without any additional structure.

Query such a structure directly is slow, so it is possible to load it into memory by
way of memory mapping then constructing an additional Rank-Select-Bit-String index.

The following code shows how to query such bit vectors and run simple queries:

```haskell
λ> import Data.Maybe
λ> import HaskellWorks.Data.Bits.BitRead
λ> import qualified HaskellWorks.Data.FromForeignRegion as IO
λ> v :: CsPoppy <- IO.mmapFromForeignRegion "data/sample-000.idx"
λ> rank1 v 100
8
λ> select1 v 8
95
```

Here the 

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
that support them.  Benchmarks were run on 2.9 GHz Intel Core i7,
macOS High Sierra.

With BMI2 disabled:

```text
benchmarking data/example.ib/CsPoppy Select1
time                 3.341 μs   (3.312 μs .. 3.385 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 3.357 μs   (3.331 μs .. 3.403 μs)
std dev              109.2 ns   (81.13 ns .. 151.9 ns)
variance introduced by outliers: 42% (moderately inflated)
```

With BMI2 enabled:

```text
benchmarking data/example.ib/CsPoppy Select1
time                 1.907 μs   (1.902 μs .. 1.911 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.906 μs   (1.901 μs .. 1.911 μs)
std dev              17.94 ns   (14.44 ns .. 21.87 ns)
```

## References

* [Space-Efficient, High-Performance Rank & Select Structures on Uncompressed Bit Sequences](http://www.cs.cmu.edu/~./dga/papers/zhou-sea2013.pdf)
