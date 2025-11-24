final: prev: {
  kent-server = final.python3Packages.toPythonApplication (
    final.python3Packages.callPackage ../packages/kent.nix { }
  );
  specify-cli = final.python3Packages.toPythonApplication (
    final.python3Packages.callPackage ../packages/specify-cli.nix { }
  );
}
