let
  baseUrl = "https://github.com/nixos/nixpkgs-channels";
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
  rev = "96069f7d890b90cbf4e8b4b53e15b036210ac146";
in

import (builtins.fetchTarball {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2020-08-10";
  url = "${baseUrl}/archive/${rev}.tar.gz";
  sha256 = "0ixyfsw7p0gq9w7hzamgnvk8xjnf62niygmpi39zh2a312k94lqr";
})
