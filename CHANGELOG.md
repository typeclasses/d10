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
