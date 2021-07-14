# d10

Data types representing the digits zero through nine.

## Modules

Each of the following modules defines a different type named `D10`, all of which are different representations of the same concept:

* `D10.Char` - Defines a `D10` type as a newtype for `Char`, where the values are restricted to characters between `'0'` and `'9'`.
* `D10.Num` - Defines a `D10` type as a newtype for any type with an instance of the `Num` class, where the values are restricted to numbers between `fromInteger 0` and `fromInteger 9`.
* `D10.Safe` - Defines a `D10` type as `D0 | D1 | D2 | ... | D9`.

## Quasi-quoters

Each module that defines a `D10` type also defines quasi-quoters for it. With the `QuasiQuotes` GHC extension enabled, a single digit like *7* can be written as `[d10|7|]`, and a list of digits like *[4,5,6]* can be written as `[d10|456|]`. For `D10.Char` and `D10.Num`, the quasi-quoters are an important feature, because the `D10` types defined in these modules have unsafe constructors, and the quasi-quoters provide compile-time assurance that we never construct a `D10` that represents a value outside the range *0* to *9*.
