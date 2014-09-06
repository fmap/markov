{ cabal }:

cabal.mkDerivation (self: {
  pname = "memoize";
  version = "0.6";
  sha256 = "15ya80la5azkpdnlnd7n6x1z9z2nixg0rakp1bj4xsk1ad1hn6x7";
  meta = {
    description = "A memoization library";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
