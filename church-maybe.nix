{ mkDerivation, base, semigroupoids, stdenv }:
mkDerivation {
  pname = "church-maybe";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base semigroupoids ];
  license = stdenv.lib.licenses.bsd3;
}
