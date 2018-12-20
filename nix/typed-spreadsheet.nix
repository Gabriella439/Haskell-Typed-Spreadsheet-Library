{ mkDerivation, async, base, diagrams-cairo, diagrams-gtk
, diagrams-lib, foldl, gtk, microlens, stdenv, stm, text
, transformers
}:
mkDerivation {
  pname = "typed-spreadsheet";
  version = "1.1.4";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base diagrams-cairo diagrams-gtk diagrams-lib foldl gtk
    microlens stm text transformers
  ];
  executableHaskellDepends = [ base diagrams-lib text ];
  description = "Typed and composable spreadsheets";
  license = stdenv.lib.licenses.bsd3;
}
