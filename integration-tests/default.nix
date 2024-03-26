{
  stdenv,
  lib,
  quickstrom,
  browser ? "chrome",
  chromedriver,
  chromium,
  geckodriver,
  firefox,
}:
let
  browserDeps =
    if browser == "chrome" then
      [
        chromedriver
        chromium
      ]
    else
      [
        geckodriver
        firefox
      ];
  browserBinary =
    if browser == "chrome" then "${chromium}/bin/chromium" else "${firefox}/bin/firefox";
  makeTest =
    {
      name,
      module,
      origin,
      include,
      options ? "",
      expectedExitCode,
    }:
    stdenv.mkDerivation {
      name = "quickstrom-integration-test-${name}";
      src = ./.;
      phases = [ "checkPhase" ];
      checkPhase = ''
        set +e
        mkdir -p $out

        echo quickstrom --log-level=INFO -I${../case-studies} -I${include} check ${module} ${origin} ${options} --browser=${browser} --browser-binary="${browserBinary}" --reporter=console --driver-log-file=driver.log | tee $out/test-report.log
        quickstrom --log-level=INFO -I${../case-studies} -I${include} check ${module} ${origin} ${options} --browser=${browser} --browser-binary="${browserBinary}" --reporter=console --driver-log-file=driver.log | tee $out/test-report.log
        exit_code=$?

        if [ $exit_code == "${toString expectedExitCode}" ]; then
            echo "Expected exit code (${toString expectedExitCode}) was returned."
        else
            cat driver.log || echo "No driver log file"
            cat interpreter.log
            echo "Expected exit code ${toString expectedExitCode}, but $exit_code was returned."
            exit 1
        fi
      '';
      doCheck = true;
      buildInputs = [
        quickstrom
        browserDeps
      ];
      __noChroot = true;
    };

  makeTests = lib.mapAttrs (name: value: makeTest (value // { inherit name; }));

  passing = name: {
    module = name;
    origin = "${./passing}/${name}.html";
    include = ./passing;
    options = "";
    expectedExitCode = 0;
  };

  failing =
    { name, expectedExitCode }:
    {
      module = name;
      origin = "${./failing}/${name}.html";
      include = ./failing;
      options = "";
      inherit expectedExitCode;
    };

in
{
  tests = makeTests {
    todomvc-backbone = {
      module = "todomvc";
      origin = "${todomvc}/examples/backbone/index.html";
      include = ./passing;
      options = "";
      expectedExitCode = 0;
    };
    liveness = passing ("liveness");
    async-change = passing ("async-change");
    async-css-change = passing ("async-css-change");
    only-noop = passing ("only-noop");
    select = passing ("select");
    invalid-spec = failing {
      name = "invalid-spec";
      expectedExitCode = 2;
    };
    scroll = passing ("scroll");
    not-interactable = failing {
      name = "not-interactable";
      expectedExitCode = 1;
    };
  };
}
