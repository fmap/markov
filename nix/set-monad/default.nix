{ cabal, deepseq }:

cabal.mkDerivation (self: {
  pname = "set-monad";
  version = "0.1.0.0";
  sha256 = "1gyzsdxl7fh99ghkb34mwm93kvfmjj5a15ws4gbshvdz6hxz3kd5";
  buildDepends = [ deepseq ];
  meta = {
    description = "Set monad";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
