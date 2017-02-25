# You can build this repository using Nix by running:
#
#     $ nix-build -A typed-spreadsheet release.nix
#
# You can also open up this repository inside of a Nix shell by running:
#
#     $ nix-shell -A typed-spreadsheet.env release.nix
#
# ... and then Nix will supply the correct Haskell development environment for
# you
let
  bootstrap = import <nixpkgs> {};

  nixpkgs =
    let
      json = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

    in
      bootstrap.fetchFromGitHub {
        owner = "NixOS";

        repo = "nixpkgs";

        inherit (json) rev sha256;
      };

  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld:
          if pkgs.stdenv.isDarwin
          then {
            gtk =
              pkgs.haskell.lib.appendConfigureFlag
                haskellPackagesOld.gtk
                "-fhave-quartz-gtk";

            typed-spreadsheet =
              pkgs.haskell.lib.overrideCabal
                (haskellPackagesNew.callPackage ./default.nix {})
                (oldDerivation: {
                    librarySystemDepends =
                      [ pkgs.darwin.apple_sdk.frameworks.Cocoa ];
                    executableSystemDepends =
                      [ pkgs.darwin.apple_sdk.frameworks.Cocoa ];
                  }
                );
          }
          else {
            typed-spreadsheet =
              haskellPackagesNew.callPackage ./default.nix {};
          };
      };
    };
  };

  pkgs =
    import nixpkgs { inherit config; };

in
  { typed-spreadsheet = pkgs.haskellPackages.typed-spreadsheet;
  }
