{
  lib,
  haskell,
  callHackageDirect,
}:

# cabal-gild >= 1.8.4 for the `fragment` pragma; nixpkgs currently ships 1.6.0.4,
# so pin the version we need directly from Hackage rather than nixpkgs' set.
(haskell.lib.compose.justStaticExecutables (
  callHackageDirect {
    pkg = "cabal-gild";
    ver = "1.8.4.1";
    sha256 = "sha256-dsHT/EKwRr5pElNJ9+RCvRLBWu6/OIJY26UZqKonRjM=";
  } { }
)).overrideAttrs
  (old: {
    meta = (old.meta or { }) // {
      description = "Format Haskell package descriptions (.cabal files)";
      homepage = "https://github.com/tfausak/cabal-gild";
      license = lib.licenses.mit;
      mainProgram = "cabal-gild";
    };
  })
