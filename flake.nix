{
  description = "text-display";

  inputs = {
    # Nix Inputs
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }: let
    utils = flake-utils.lib;
  in
    utils.eachDefaultSystem (system: let
      compilerVersion = "ghc944";
      pkgs = nixpkgs.legacyPackages.${system};
      hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
        overrides = hfinal: hprev: {
          # Internal Packages
          book = hfinal.callCabal2nix "book" ./. {};
        };
      };
    in {
      # nix develop
      devShell = hsPkgs.shellFor {
        name = "text-display";
        shellHook = ''
          export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
          export LC_ALL=C.UTF-8
        '';

        packages = p: [
          p.book
        ];

        buildInputs = with pkgs;
          [
            # Haskell Deps
            cabal-install
            hlint
            haskellPackages.apply-refact
            haskellPackages.fourmolu_0_12_0_0
            haskellPackages.cabal-fmt
            haskell-language-server
            ghc

            # DB Deps
            gmp
            zlib
            glibcLocales

            # Extra
            parallel
            git
            gnumake
          ];
      };

      # nix build
      packages = {
        book = hsPkgs.book;
      };

      # nix run
      apps = {
        book = utils.mkApp {
          drv = self.packages.${system}.book;
        };
      };
    });
}
