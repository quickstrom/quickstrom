import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2020-08-10";
  url = "https://github.com/nixos/nixpkgs-channels/";
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
  ref = "refs/heads/nixos-unstable";
  rev = "96069f7d890b90cbf4e8b4b53e15b036210ac146";
})