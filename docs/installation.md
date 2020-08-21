# Installation

Follow these steps to install Quickstrom on your machine. For the moment, the
only supported installation method is Nix. Other options might be described
in the future.

## Prerequisites

- Nix (see [nix.dev](https://nix.dev/) for installation instructions and guides)

## From Source

First, clone and enter the repository:

```shell
git clone git@github.com:quickstrom/quickstrom.git
cd quickstrom
```

To install the `quickstrom` executable, use Cachix and Nix to get an
executable:

```shell
cachix use quickstrom # optional, but good if you don't like waiting
nix-build
```

Now, Quickstrom is available in the `result`:

```shell
result/bin/quickstrom check <YOUR SPEC FILE> <ORIGIN URL>
```

Alternatively, install it directly into your environment:

```shell
nix-env -i -A quickstrom -f default.nix
quickstrom check <YOUR SPEC FILE> <ORIGIN URL>
```

**NOTE:** You need to also run geckodriver for Quickstrom to work. See [Running](#running) for more information how to
run Quickstrom.