{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, diagrams-cairo, diagrams-gtk
      , diagrams-lib, foldl, gtk, microlens, stdenv, stm, text
      , transformers
      }:
      mkDerivation {
        pname = "typed-spreadsheet";
        version = "1.1.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          async base diagrams-cairo diagrams-gtk diagrams-lib foldl gtk
          microlens stm text transformers
        ];
        executableHaskellDepends = [
          base diagrams-cairo diagrams-lib text
        ];
        description = "Typed and composable spreadsheets";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
