let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
    rev = "6a7dea9330c3d1f1f53610e753aada029eb8b86e";

    sha256 = "0i8nf8szg27vnha8l22k9wwj3fyya6mf4b6g05fi1kyv3mmazhq7";
  };

  readDirectory = import ./nix/readDirectory.nix;

  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides =
          let
            manualOverrides = haskellPackagesNew: haskellPackagesOld: {
              typed-spreadsheet =
                if pkgs.stdenv.isDarwin
                then
                  pkgs.haskell.lib.addBuildDepend
                    haskellPackagesOld.typed-spreadsheet
                    pkgs.darwin.apple_sdk.frameworks.Cocoa
                else
                  haskellPackagesOld.typed-spreadsheet;
            };

          in
            pkgs.lib.composeExtensions (readDirectory ./nix) manualOverrides;
      };
    };
  };

  pkgs =
    import nixpkgs { inherit config; };

in
  { inherit (pkgs.haskellPackages) typed-spreadsheet;

    shell = (pkgs.haskell.lib.doBenchmark pkgs.haskellPackages.typed-spreadsheet).env;
  }
