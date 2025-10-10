{
  lib,
  # python packaging utils
  buildPythonPackage,
  fetchPypi,
  pytestCheckHook,
  # build system dependencies
  setuptools,
  setuptools-scm,
  # library dependencies
  flask,
}:

let
  finalAttrs = {
    pname = "kent";
    version = "2.1.0";
    pyproject = true;

    src = fetchPypi {
      inherit (finalAttrs) pname version;
      hash = "sha256-i4c+2YuHQKpsIcW72DrWTTYr02FLSL2xPTPZm9APSoI=";
    };

    build-system = [
      setuptools
      setuptools-scm
    ];

    dependencies = [ flask ];

    nativeCheckInputs = [ pytestCheckHook ];
    pythonImportsCheck = [ "kent" ];

    meta = {
      changelog = "https://github.com/mozilla-services/kent/blob/v${finalAttrs.version}/HISTORY.rst";
      homepage = "https://github.com/mozilla-services/kent";
      description = "Fake Sentry server for local development, debugging, and integration testing";
      licenses = lib.licenses.mpl20;
      mainProgram = "kent-server";
    };
  };
in
buildPythonPackage finalAttrs
