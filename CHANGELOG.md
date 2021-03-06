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

  * Removed `isD10Str` (see `Data.D10.Predicate`)
  * Type of `d10Exp` changed
    from `Integral a => a -> Q Exp`
    to `Integer -> Q Exp`
  * Type of `d10Pat` changed
    from `D10 -> Q Pat`
    to `Integer -> Q Pat`
  * Type of `d10ListPat` changed
    from `[D10] -> Q Pat`
    to `String -> Q Pat`

In module `Data.D10.Num`:

  * Removed `isD10Str` (see `Data.D10.Predicate`)
  * Type of `d10Exp` changed
    from `(Integral b, Lift a, Num a) => b -> Q Exp`
    to `Integer -> Q Exp`
  * Type of `d10ListExp` changed
    from `(Lift a, Num a) => String -> Q Exp`
    to `String -> Q Exp`
  * Type of `d10Pat` changed
    from `Integral a => D10 a -> Q Pat`
    to `Integer -> Q Pat`
  * Type of `d10ListPat` changed
    from `Integral a => [D10 a] -> Q Pat`
    to `String -> Q Pat`
  * Type of `d10` changed
    from `(Lift a, Integral a) => QuasiQuoter`
    to `QuasiQuoter`
  * Type of `d10list` changed
    from `(Lift a, Integral a) => QuasiQuoter`
    to `QuasiQuoter`
  * Although type variables no longer appear in the
    various Template Haskell functions, the expressions
    and patterns they generate are polymorphic.

In module `Data.D10.Safe`:

  * The `D10` type now has instances of the `Data` and `Generic`.
  * Type of `d10ListPat` changed
    from `[D10] -> Q Pat`
    to `String -> Q Pat`
  * Removed `d10Exp`, `d10Pat`, and `d10`

Other changes:

  * `doctest` test dependency has been removed, and
    `hedgehog` dependency has been added instead

**v0.3.0.1**

  * Support GHC 9.0, base 4.15, template-haskell 2.17

**v1**

Removed the `Data` prefix from module names.

  * `Data.D10.Char` is now `D10.Char`
  * `Data.D10.Num` is now `D10.Num`
  * `Data.D10.Predicate` is now `D10.Predicate`
  * `Data.D10.Safe` is now `D10.Safe`

Constructors for `D10` are no longer exported by the modules formerly
known as `Data.D10.Char` and `Data.D10.Num`. They have moved to the
new modules `D10.Char.Unsafe` and `D10.Num.Unsafe` respectively.

Operators `(+)`, `(-)`, and `(*)` have been moved into their own
separate modules, as the names conflict with `Prelude` functions.
These new modules are:

  * `D10.Char.Arithmetic`
  * `D10.Num.Arithmetic`
  * `D10.Safe.Arithmetic`

Other new modules:

  * `D10.Char.Conversions`
  * `D10.Char.Quotes`
  * `D10.Char.Splices`
  * `D10.Char.Type`
  * `D10.Num.Conversions`
  * `D10.Num.Quotes`
  * `D10.Num.Splices`
  * `D10.Num.Type`
  * `D10.Safe.Conversions`
  * `D10.Safe.Quotes`
  * `D10.Safe.Splices`
  * `D10.Safe.Type`

All instances of the `Language.Haskell.TH.Syntax.Lift` class
have been removed

Added *Safe Haskell* language flags (`Safe`, `Trustworthy`, `Unsafe`)
to indicate which modules permit constructing invalid values

Required Cabal version required is raised from 2.2 to 3.0

**v1.0.0.1**

Remove `README.md` from the Cabal package
