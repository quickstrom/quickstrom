# WebCheck

## Testing

To run the tests, you need to have a built version of the
[purescript-webcheck](purescript-webcheck) package, and set the
`PURESCRIPT_WEBCHECK` environment variable to the package's output
directory.

```
PURESCRIPT_WEBCHECK=purescript-webcheck/output cabal test
```

The good news is, if you're running a `nix-shell` from the root of
this project, that's all taken care of for you! The
[purescript-webcheck](purescript-webcheck) package is then prebuilt and
exposed with an environment variable in your shell.

If you're hacking on the PureScript package, as described in [its
instructions](purescript-webcheck/README.md), you might want to use
the local output directory anyway. If so, use the command in the
snippet above after having built the package with Spago.
