# purescript-webcheck

This is the user-level API for writing specifications in PureScript.

## Requirements

- Nix

## Building

A regular build, for use in WebCheck, is done with Nix:

```
$ nix-build
```

This creates a derivation with compiled externs and CoreFn
representations of all dependencies, including the WebCheck modules in
this package.

WebCheck's Nix build invokes this build itself, so you generally don't
need to run `nix-build` for this package.

## Hacking

First, install the packages in the Spago directory.


```
$ nix-shell
...
$ spago2nix install -j 100
$ spago build -u '-g corefn'
```

Now all dependencies should be in the standard Spago location, so that
Spago and the PureScript IDE works as usual. Launch your editor and use
this package as the project root, and it should work nicely!

## Generating Spago Packages

If the dependencies or package set change, the Spago packages file for
Nix needs to be regenerated:

```
$ nix-shell
...
$ spago2nix generate
```
