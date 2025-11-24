{
  lib,
  # packaging utils
  fetchFromGitHub,
  buildPythonPackage,
  # build systemd ependencies
  hatchling,
  hatch-vcs,
  # library dependencies
  typer,
  rich,
  httpx,
  platformdirs,
  readchar,
  truststore,
}:

let
  finalAttrs = {
    pname = "specify";
    version = "0.0.85";
    pyproject = true;

    src = fetchFromGitHub {
      owner = "github";
      repo = "spec-kit";
      tag = "v${finalAttrs.version}";
      hash = "sha256-h4QPGg7KilfxzkWf1Hrk4bkveapKRkbzeEhioxdx1do=";
    };

    build-system = [
      hatchling
      hatch-vcs
    ];

    dependencies = [
      typer
      rich
      httpx
      platformdirs
      readchar
      truststore
    ];

    meta = {
      description = "CLI tool to bootstraps projects for SDD and works with AI coding agents";
      homepage = "https://github.com/github/spec-kit";
      license = lib.licenses.mit;
    };
  };
in
buildPythonPackage finalAttrs
