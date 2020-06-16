# WebCheck

## Prerequisites

## Testing

To run the tests, you need to have a built version of the
[purescript-webcheck](purescript-webcheck) package, and set the
`PURESCRIPT_WEBCHECK` environment variable to the package's output
directory.

The good news is, if you're running a `nix-shell` from the root of
this project, that's all taken care of for you! The
[purescript-webcheck](purescript-webcheck) package is then prebuilt and
exposed with an environment variable in your shell.

To run the tests:

```
cabal test
```

If you're hacking on the PureScript package, as described in [its
instructions](purescript-webcheck/README.md), you might want to use the local
output directory anyway. If so, use the following command after having built
the package with Spago.

```
WEBCHECK_LIBRARY_DIR=purescript-webcheck/output cabal test
```