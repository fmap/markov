{ cabal, ansiTerminal, interpolate }:

cabal.mkDerivation (self: {
  pname = "assertions";
  version = "0.1.0.4";
  sha256 = "1b2p6b6brk0b1hq264i20bpdhdaq4xdzcqp7gzvfy1s5q3zwjzj8";
  buildDepends = [ ansiTerminal ];
  testDepends = [ interpolate ];
  doCheck = false;
  meta = {
    description = "A simple testing framework";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
