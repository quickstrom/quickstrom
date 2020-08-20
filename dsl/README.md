# Quickstrom DSL

This is the DSL used by testers when writing specifications. It's based on
PureScript.

## Requirements

- Nix

## Building

A regular build, for use in Quickstrom, is done with Nix:

```
$ nix-build
```

This creates a derivation with compiled externs and CoreFn
representations of all dependencies, including the Quickstrom modules in
this package.

Quickstrom's Nix build invokes this build itself, so you generally don't
need to run `nix-build` for this package.

## Hacking

First, install the packages in the Spago directory.


```
$ nix-shell
...
$ spago2nix install -j 100
```

Now all dependencies should be in the standard Spago location, so that
Spago and the PureScript IDE works as usual. Launch your editor and use
this package as the project root, and it should work nicely!

You can build the Quickstrom libraries manually with spago. This is useful if
you're hacking on these modules (or the specifications in `specs/`) and want
to run Quickstrom against your working copy (see `QUICKSTROM_LIBRARY_DIR` in the
root README).

```
$ spago build -u '-g corefn'
```

## Generating Spago Packages

If the dependencies or package set change, the Spago packages file for
Nix needs to be regenerated:

```
$ nix-shell
...
$ spago2nix generate
```
