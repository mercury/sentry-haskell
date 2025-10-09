{
  description = "";
  outputs =
    inputs@{ self, nixpkgs }:
    let
      inherit (import ./nix/dev/util.nix { inherit inputs; })
        forEachSystem
        loadPrivateFlake
        ;
      privateInputs = (loadPrivateFlake ./nix/dev/private).inputs;
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
        "aarch64-linux"
      ];
      eachSystem = forEachSystem systems;
    in
    {
      # public outputs, should not reference

      # (private) dev outputs, should not be imported by downstream consumers
      # as they may depend on private flake inputs.
      devShells = eachSystem (
        { pkgs, ... }:
        {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [ ];
          };
        }
      );
      apps = eachSystem (
        { pkgs, ... }:
        {
          update-dev-private-narHash = {
            type = "app";
            program = "${pkgs.writeShellScript "update-dev-private-narHash" ''
              nix --extra-experimental-features "nix-command flakes" flake lock ./dev/private
              nix --extra-experimental-features "nix-command flakes" hash path ./dev/private | tr -d '\n' > ./dev/private.narHash
            ''}";
          };
        }
      );
      formatter = eachSystem (
        { pkgs, ... }:
        let
          eval = cfg: (privateInputs.treefmt-nix.lib.evalModule pkgs cfg).config.build.wrapper;
        in
        eval {
          projectRootFile = "flake.nix";
          programs.nixfmt.enable = true;
          # Globally exclude the following patterns from auto-formatting:
          settings.global.excludes = [
            # ...jujutsu's VCS tracking directory
            ".jj/*"
            # ...all markdown files
            "*.md"
          ];
        }
      );
    };
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
}
