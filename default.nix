{ cabal, aeson, ansiWlPprint, deepseq, doctest, hspec, iso8601Time
, lens, mtl, text, time, trifecta, unorderedContainers, utf8String
, vector
}:

cabal.mkDerivation (self: {
  pname = "toml";
  version = "0.1.0.0";
  sha256 = "1cjq5rckhzqpi8iayx2xnqgdng3qldpd1rssv080paqlikiqx0c2";
  src = ./.;
  buildDepends = [
    aeson ansiWlPprint deepseq iso8601Time lens mtl text time trifecta
    unorderedContainers utf8String vector
  ];
  testDepends = [ doctest hspec ];
  meta = {
    description = "TOML parser";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
