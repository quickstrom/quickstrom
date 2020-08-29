# Contributing

Hey! Do you want to contribute to Quickstrom? Using Nix for development is
highly encouraged when working on Quickstrom. The guide below assumes you're
OK with this. You can make contributions without using Nix, but other setups
are not documented yet.

## Contributor License Agreement (CLA)

First, let's get the boring thing out of the way. When submitting a PR to
Quickstrom, you'll be requested to sign a CLA. Quickstrom might be used in a
commercial project in the future, which is why it's of interest to retain
copyright and to have flexibility around licensing. The plan is to always
have an open-source core and command-line version of Quickstrom available
under AGPL-3.0, though.

## Developing with Nix

Quickstrom is developed with Nix. You might get away with non-Nix tools, but
its the only blessed setup for now.

First, set up the Quickstrom cache in Cachix:

```
cachix use quickstrom
```

**Do not skip this step.** It takes forever to build otherwise.

### Starting a Shell

All subsequent commands in the document assume you're in the project's Nix
shell. Either run the following command from the project root:

```
nix-shell
```

Or use [Lorri](https://github.com/target/lorri) and
[direnv](https://direnv.net/) for an automatic and reloading environment
(highly recommended!).

```
lorri shell # --cached
```

## Testing

(Testing Quickstrom itself, that is. Not web applications using
specifications.)

To run the tests, you need to have a built version of the
[dsl](dsl) package, and set the
`QUICKSTROM_LIBRARY_DIR` environment variable to the package's output
directory.

The good news is, if you're running a `nix-shell` from the root of
this project, that's all taken care of for you! The
[dsl](dsl) package is then prebuilt and
exposed with an environment variable in your shell.

To run the tests:

```
cabal test
```

If you're hacking on the PureScript package, as described in [its
instructions](dsl/README.md), you might want to use the local
output directory anyway. If so, use the following command after having built
the package with Spago.

```
QUICKSTROM_LIBRARY_DIR=dsl/output cabal test
```

## Formatting

In a Nix shell, run the following command to format all Haskell and
PureScript sources:

```shell
quickstrom-format-sources
```

## Documentation

Documentation is built with Sphinx. The project Nix shell should give you all
you need. You can then run `make html` to build it locally:

```shell
cd docs
make html
```

To deploy the built documentation, run:

```shell
nix-shell --run quickstrom-publish-docs
```
