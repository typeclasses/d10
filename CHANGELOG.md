**v0.1.0.0**

  * Initial release

**v0.1.0.1**

  * Improve error messages when quasi-quoters are used in a
    non-expression context

**v0.1.1.0**

  * Add functions for generating Template Haskell expressions
    to be spliced, as alternatives to using the quasi-quoters

**v0.2.0.0**

  * Add Template Haskell splice patterns `d10Pat` and `d10ListPat`
  * Define `quotePat` for the quasi-quoters `d10` and `d10list`,
    so they can now be used with pattern matching
  * Add `Integral a` constraint to `d10` and `d10list` in the
    `Data.D10.Num` module, because this is needed for the
    definition of `quotePat`.

**v0.2.0.1**

  * Add `CHANGELOG.md` to package distribution

**v0.2.1.0**

  * Add functions mod-10 arithmetic functions: `(+)`, `(-)`, `(*)`

**v0.2.1.2**

  * Support GHC 8.8

**v0.2.1.4**

  * Support GHC 8.10

**v0.2.1.6**

  * Support `doctest-0.17`

**v0.3**

In module `Data.D10.Char`:

  * Removed `isD10Str` (This function is still
    available in `Data.D10.Predicate`)
  * Type of `d10Exp` changed from
    `Integral a => a -> Q Exp` to `Integer -> Q Exp`
  * Type of `d10Pat` changed from
    `D10 -> Q Pat` to `Integer -> Q Pat`
  * Type of `d10ListPat` changed from
    `[D10] -> Q Pat` to `String -> Q Pat`

In module `Data.D10.Num`:

  * Removed `isD10Str` (This function is still
    available in `Data.D10.Predicate`)
  * Type of `d10Exp` changed from
    `Integral a => a -> Q Exp` to `Integer -> Q Exp`
  * Type of `d10Pat` changed from
    `Integral a => D10 a -> Q Pat` to `Integer -> Q Pat`

In module `Data.D10.Safe`:

  * The `D10` type now has instances of the `Data` and `Generic`.
  * Type of `d10Exp` changed from
    `Integral a => a -> Q Exp` to `Integer -> Q Exp`
  * Type of `d10ListPat` changed from
    `[D10] -> Q Pat` to `String -> Q Pat`
  * `d10Pat` has been removed.
