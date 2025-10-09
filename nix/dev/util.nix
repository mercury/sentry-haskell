{ inputs }:
let
  loadPrivateFlake =
    path:
    let
      flakeHash = builtins.readFile "${toString path}.narHash";
      flakePath = "path:${toString path}?narHash=${flakeHash}";
    in
    builtins.getFlake (builtins.unsafeDiscardStringContext flakePath);
  forEachSystem =
    systems: f:
    builtins.listToAttrs (
      builtins.map (system: {
        name = system;
        value = f {
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          inherit system;
        };
      }) systems
    );
in
{
  inherit forEachSystem loadPrivateFlake;
}
