# Contributing

Hey! Do you want to contribute to Quickstrom? Using Nix for development is
highly encouraged when working on Quickstrom. The guide below assumes you're
OK with this. You can make contributions without using Nix, but other setups
are not documented yet.

## Developing with Nix

Quickstrom is developed with Nix. You might get away with non-Nix tools, but
its the only blessed setup for now.

First, set up the Quickstrom cache in Cachix:

```
cachix use quickstrom
```

**Do not skip this step.** It saves a lot of your time.


## Development Setup

The developer environment is provided by this project's Nix shell.

First, install:

* [Nix](https://nixos.org/download.html)
* [direnv](https://direnv.net/) (make sure to add the shell hook)

Next, set up direnv in the repository:

```shell
echo "use nix" > .envrc
direnv allow .
```

### MacOS

Some dependencies do not work on macOS 11 (at least with the M1 processors). To run browser tests, install Google Chrome or Firefox by some other method, e.g. Homebrew.

Further, the integration tests and main Nix derivation can't currently be built on macOS. The Nix shell works and should be sufficient for most development.

## Check a TodoMVC Implementation

```shell
poetry run quickstrom -Iulib -Icase-studies --log-level=debug check todomvc $(nix eval --file ./case-studies/todomvc.nix --raw '')/examples/dojo/index.html --browser=chrome --capture-screenshots
```

## Run Integration Tests

```shell
nix build -f integration-tests/default.nix --option sandbox relaxed
```

## Typechecking

This project uses Pyright. Typecheck all files like so:

```shell
poetry run pyright quickstrom
```
