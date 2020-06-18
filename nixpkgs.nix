import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2020-06-18";
  url = "https://github.com/nixos/nixpkgs-channels/";
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
  ref = "refs/heads/nixos-unstable";
  rev = "22c98819ccdf042b30103d827d35644ed0f17831";
})