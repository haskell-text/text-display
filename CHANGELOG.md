# CHANGELOG

## [v1.0.0.0] - 21/07/2024

* Remove support for GHC 8.10 to 9.0 inclusive
* `text-display` is now backed by [`text-builder-linear`](https://flora.pm/packages/@hackage/text-builder-linear)
  * Manual instances will need some adjustments
  * As a result of the builder's strictness properties, lazy evaluation of lists is dropped
  * Derived instances should not require adjustments

## [v0.0.5.2] - 07/04/2024

* Allow GHC 9.6.4 and 9.8.2

## [v0.0.5.1] - 04/11/2023

*  Allow bytestring 0.12 (for GHC 9.8) [#58](https://github.com/haskell-text/text-display/pull/58)

## [v0.0.5.0] - 16/05/2023

* Add Generic machinery (GDisplay1) and RecordInstance newtype for deriving via (https://github.com/haskell-text/text-display/pull/52)

## [v0.0.4.0] - 21/03/2023

* Fix `displayList` by making it lazier (https://github.com/haskell-text/text-display/pull/27)
* Add Display instance for `Void` (https://github.com/haskell-text/text-display/pull/28)

## [v0.0.3.0] - 21/08/2022

This is an experimental release.

* Support for text-2.0
* Support for GHC 9.4

## [v0.0.2.0] – 13/03/2022

This is an experimental release.

* Typo corrections, documentation improvements
* Fix the String instance (https://github.com/haskell-text/text-display/pull/17)
* Bump dependencies

## [v0.0.1.0] – 2/11/2021

This is an experimental release.

* Initial release

[Unreleased]: https://github.com/kleidukos/text-display/compare/v0.0.1.0...HEAD
[v0.0.1.0]: https://github.com/kleidukos/text-display/releases/tag/v0.0.1.0
[v0.0.2.0]: https://github.com/kleidukos/text-display/releases/tag/v0.0.2.0
[v0.0.3.0]: https://github.com/kleidukos/text-display/releases/tag/v0.0.3.0
