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

  * `Data.D10.Char` and `Data.D10.Num` no longer export `isD10Str`.
    This function is still available in `Data.D10.Predicate`.

  * The `D10` type in `Data.D10.Safe` now has instances of the
    `Data` and `Generic` classes.

  * The type of `d10Exp` has changed from

    ```haskell
    d10Exp :: Integral a => a -> Q Exp
    ```

    to

    ```haskell
    d10Exp :: Integer -> Q Exp
    ```

    in modules `Data.D10.Char`, `Data.D10.Num`, and `Data.D10.Safe`.

  * The type of `d10Pat` has changed from

    ```haskell
    d10Pat :: D10 -> Q Pat
    ```

    to

    ```haskell
    d10Pat :: Integer -> Q Pat
    ```

    in module `Data.D10.Char`.

  * The type of `d10Pat` has changed from

    ```haskell
    d10Pat :: Integral a => D10 a -> Q Pat
    ```

    to

    ```haskell
    d10Pat :: Integer -> Q Pat
    ```

    in module `Data.D10.Num`.

  * `d10Pat` has been removed from module `Data.D10.Safe`.
