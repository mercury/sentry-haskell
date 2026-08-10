# Dev-only overlay: intentionally NOT exported from `self.overlays` (the public
# surface consumed by downstream flakes), only applied to the local devShell.
final: _prev: {
  cabal-gild = final.haskellPackages.callPackage ../packages/cabal-gild.nix { };
}
