# Contributing

Hi, and thank you very much for contribution to `text-display`!  
You will find below guidelines for contributing to the project

## Supported toolchains

The project explicitly supports the following versions of GHC that are listed in [text-display.cabal](https://github.com/haskell-text/text-display/blob/main/text-display.cabal).

Other versions may be incidentally supported but we offer no guarantee on the
stability of this support.

## Code style

Please run `make style` and `make lint` on your patches to conform to the coding style
in effect. The CI will guard against deviations.

## Additions and changes to the API

Additions and changes to the API must be documented in the CHANGELOG.md file.
Do not forget to add `@since` metadata in your Haddock comments when a function
or type is introduced or has changed significantly for API users.  

## Documentation

Pull Requests that change the observable API must include documentation.
The precise shape of it is determined on a case-by-case basis, but be receptive
to the feedback from the development team.
