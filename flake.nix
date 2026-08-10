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

      ghcVersions = [
        "ghc910"
        "ghc912"
      ];

      overlays = [
        self.overlays.native
        self.overlays.development
      ];

      eachSystem = forEachSystem { inherit systems overlays; };

      ciPkgsFor =
        pkgs: ghc:
        [ (pkgs.haskell.packages.${ghc}.ghcWithPackages (import ./nix/dev/haskell-deps.nix)) ]
        ++ (with pkgs; [
          cabal-gild
          cabal-install
          just
          kent-server
          pkg-config
          zlib.dev
          zstd.dev
        ]);

      devPkgsFor =
        pkgs:
        (with pkgs; [
          ghciwatch
          # end-to-end wall-clock benchmarking for the profile harness
          hyperfine
        ])
        ++ (with pkgs.haskellPackages; [
          # cost-centre (.prof) analysis
          profiterole
          profiteur
          ghc-prof-flamegraph
          # eventlog + heap-profile rendering (browser-based, cross-platform)
          eventlog2html
        ]);
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
          default = pkgs.mkShell { buildInputs = ciPkgsFor pkgs "ghc910" ++ devPkgsFor pkgs; };
        }
        // builtins.listToAttrs (
          map (ghc: {
            name = "ci-${ghc}";
            value = pkgs.mkShell { buildInputs = ciPkgsFor pkgs ghc; };
          }) ghcVersions
        )
      );

      # (private) dev outputs: CI toolchain as a single Nix package
      packages = eachSystem (
        { pkgs, ... }:
        (builtins.listToAttrs (
          map (ghc: {
            name = "ci-deps-${ghc}";
            value = pkgs.linkFarmFromDrvs "sentry-haskell-ci-deps-${ghc}" (ciPkgsFor pkgs ghc);
          }) ghcVersions
        ))
        // {
          pinact = pkgs.pinact;
        }
      );

      apps = eachSystem (
        { pkgs, ... }:
        {
          # Run from the repo root: paths below are relative to the CWD.
          update-dev-private-narHash = {
            type = "app";
            program = "${pkgs.writeShellScript "update-dev-private-narHash" ''
              nix --extra-experimental-features "nix-command flakes" flake lock ./nix/dev/private
              nix --extra-experimental-features "nix-command flakes" hash path ./nix/dev/private | tr -d '\n' > ./nix/dev/private.narHash
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
