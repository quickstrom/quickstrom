{ mkShell, 
  quickstrom,
  chromedriver,
  chromium,
  geckodriver,
  firefox,
}:
let

  todomvc = builtins.fetchTarball {
    url = "https://github.com/tastejs/todomvc/archive/41ba86db92336c11e56d425c5151b7ec2932be9a.tar.gz";
    sha256 = "sha256:1kwpzsslp8cnmdp435syjcfwn54f3fqssniwhr5lynv0a8sqplnx";
  };
in
mkShell {
  packages = [ quickstrom chromedriver chromium geckodriver firefox ];

  shellHook = ''
    export TODOMVC_DIR=${todomvc}
  '';
}
