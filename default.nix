{ mkDerivation, base, mtl, stdenv, transformers, xhb, xhb-requests
}:
mkDerivation {
  pname = "xhb-monad";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [ base mtl transformers xhb xhb-requests ];
  license = stdenv.lib.licenses.mit;
}
