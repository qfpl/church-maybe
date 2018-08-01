{ mkDerivation, base, deepseq, semigroupoids, semigroups, stdenv }:
mkDerivation {
  pname = "church-maybe";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base deepseq semigroupoids semigroups ];
  license = stdenv.lib.licenses.bsd3;
}
