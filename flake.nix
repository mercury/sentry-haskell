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
      overlays = [
        self.overlays.native
        self.overlays.development
      ];
      eachSystem = forEachSystem { inherit systems overlays; };
    in
    {
      # public outputs, should not reference private flake inputs.
      overlays = {
        development = import ./nix/overlays/development.nix;
        native = import ./nix/overlays/native.nix;
      };

      # (private) dev outputs, should not be imported by downstream consumers
      # as they may depend on private flake inputs.
      devShells = eachSystem (
        { pkgs, ... }:
        {
          default = pkgs.mkShell {
            buildInputs =
              (with pkgs; [
                # tooling
                cabal-gild
                cabal-install
                ghciwatch
                haskell.compiler.ghc910
                just
                # integration test server
                kent-server
                # end-to-end wall-clock benchmarking for the profile harness
                hyperfine
                # C library dependencies
                zlib.dev
              ])
              ++ (with pkgs.haskell.packages.ghc910; [
                # cost-centre (.prof) analysis
                profiterole
                profiteur
                ghc-prof-flamegraph
                # eventlog + heap-profile rendering (browser-based, cross-platform)
                eventlog2html
              ]);
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
          programs = {
            cabal-gild = {
              enable = true;
              package = pkgs.cabal-gild;
            };
            fourmolu.enable = true;
            nixfmt.enable = true;
          };
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
