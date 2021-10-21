# Contributing

Hi, and thank you very much for contribution to `text-display`!  
You will find below guidelines for contributing to the project

## Supported toolchains

The project explicitly supports the following versions of GHC:

* 8.8.4
* 8.10.7
* 9.0.1

Other versions may be incidentally supported but we offer no guarantee on the stability of this support.

The minimal version of Cabal used is 3.0, as indicated in the `text-display.cabal` file.

## Running HLint and tests

The `Makefile` has two rules - `check` and `nix-check` that will run HLint and the test suite.
Running these rules before pushing your code may guard you against surprises during the CI.

## Additions and changes to the API

Additions and changes to the API must be documented in the CHANGELOG.md file. Do not forget to add `@since` metadata
in your Haddock comments when a function or type is introduced or has changed significantly for API users.  

## Documentation

Pull Requests that change the observable API must include documentation. The precise shape of it is determined on a
case-by-case basis, but be receptive to the feedback from the development team.
