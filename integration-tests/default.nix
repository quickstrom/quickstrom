{ pkgs ? import ../nixpkgs.nix { config = { allowBroken = true; }; } }:
let
  quickstrom = (import ../. { }).quickstrom;

  browsers = {
    firefox = {
      name = "firefox";
      binary = "/home/vlad/.nix-profile/bin/firefox";
      server =
        "geckodriver --host 127.0.0.1 --port 4444 --marionette-host 127.0.0.1 --marionette-port 4445";
      buildInputs = [pkgs.geckodriver pkgs.firefox];
    };
    chromium = {
      name = "chrome";
      binary = "${pkgs.chromium}/bin/chromium-browser";
      server = "${pkgs.chromedriver}/bin/chromedriver --port=4444";
      buildInputs = [pkgs.chromedriver pkgs.chromium];
    };
  };

  makeTest = { name, spec, origin, browser, options ? "", expectedExitCode }:
    pkgs.stdenv.mkDerivation {
      name = "quickstrom-integration-test-${name}";
      src = ./.;
      unpackPhase = "true";
      buildPhase = "true";
      checkPhase = ''
        set +e
        mkdir -p $out

        HOME=/tmp ${browser.server} &
        webdriver_pid="$!"
        trap "kill $webdriver_pid" EXIT

        quickstrom check ${spec} ${origin} ${options} --log-level=INFO --browser=${browser.name} --browser-binary="${browser.binary}" | tee $out/test-report.log
        exit_code=$?

        if [ $exit_code == "${toString expectedExitCode}" ]; then
            echo "Expected exit code (${
              toString expectedExitCode
            }) was returned."
        else 
            echo "Expected exit code ${
              toString expectedExitCode
            }, but $exit_code was returned."
            exit 1
        fi
      '';
      doCheck = true;
      buildInputs = [ quickstrom ] ++ browser.buildInputs;
      installPhase = "true";
      __noChroot = browser.name == "chrome";
    };

  makeTests =
    pkgs.lib.mapAttrs (name: value: makeTest (value // { inherit name; }));

  todomvc = builtins.fetchTarball {
    url =
      "https://github.com/tastejs/todomvc/archive/41ba86db92336c11e56d425c5151b7ec2932be9a.tar.gz";
    # nix-prefetch-url --unpack <url>
    sha256 = "1kwpzsslp8cnmdp435syjcfwn54f3fqssniwhr5lynv0a8sqplnx";
  };

in makeTests {
  toggle = {
    spec = ./passing/Toggle.spec.purs;
    origin = "$src/passing/Toggle.html";
    options = "--max-actions=50";
    browser = browsers.firefox;
    expectedExitCode = 0;
  };
  until = {
    spec = ./passing/Until.spec.purs;
    origin = "$src/passing/Until.html";
    options = "--max-actions=50";
    browser = browsers.firefox;
    expectedExitCode = 0;
  };
  ReactButton = {
    spec = ./passing/ReactButton.spec.purs;
    origin = "$src/passing/ReactButton.html";
    options = "--max-actions=50";
    browser = browsers.firefox;
    expectedExitCode = 0;
  };
  MultiPage = {
    spec = ./passing/MultiPage.spec.purs;
    origin = "$src/passing/MultiPage.html";
    options = "--max-actions=50";
    browser = browsers.firefox;
    expectedExitCode = 0;
  };
  todomvc-vue = {
    spec = ./other/TodoMVC.spec.purs;
    origin = "${todomvc}/examples/vue/index.html";
    options = "--max-trailing-state-changes=0";
    browser = browsers.firefox;
    expectedExitCode = 0;
  };

  todomvc-angularjs = {
    spec = ./other/TodoMVC.spec.purs;
    origin = "${todomvc}/examples/angularjs/index.html";
    options = "--shrink-levels=0";
    browser = browsers.firefox;
    expectedExitCode = 3;
  };
  AsyncUpdate = {
    spec = ./failing/AsyncUpdate.spec.purs;
    origin = "$src/failing/AsyncUpdate.html";
    options = "--shrink-levels=0 --tests=50 --max-actions=100";
    browser = browsers.firefox;
    expectedExitCode = 3;
  };
  DoubleToggle = {
    spec = ./failing/DoubleToggle.spec.purs;
    origin = "$src/failing/DoubleToggle.html";
    options = "--shrink-levels=0 --tests=50 --max-actions=100";
    browser = browsers.firefox;
    expectedExitCode = 3;
  };
  # This one is disabled due to flakiness:
  #
  # comment-form = {
  #   spec = ./failing/CommentForm.spec.purs;
  #   origin = "$src/failing/CommentForm.html";
  #   options = "--shrink-levels=0 --tests=50 --max-actions=100";
  #   browser = browsers.firefox;
  #   expectedExitCode = 3;
  # };
  Spinners = {
    spec = ./failing/Spinners.spec.purs;
    origin = "$src/failing/Spinners.html";
    options = "--shrink-levels=0 --tests=50 --max-actions=100";
    browser = browsers.chromium;
    expectedExitCode = 3;
  };

  # This type of file upload (regular button + dynamically created file input)
  # is used to create custom file inputs, for instance by the elm/file library.
  FileUpload-firefox = {
    spec = ./other/FileUpload.spec.purs;
    origin = "$src/other/FileUpload.html";
    options = "--shrink-levels=0 --tests=50 --max-actions=100";
    browser = browsers.firefox;
    # Firefox through Geckodriver doesn't support the custom file input
    # technique, and crashes when the triggering button is clicked.
    expectedExitCode = 1;
  };
  FileUpload-chromium = {
    spec = ./other/FileUpload.spec.purs;
    origin = "$src/other/FileUpload.html";
    options = "--shrink-levels=0 --tests=10 --max-actions=100";
    browser = browsers.chromium;
    # Chrome/Chromium and Chromedriver, however, support it.
    expectedExitCode = 0;
  };
}
