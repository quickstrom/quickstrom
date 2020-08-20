<div align=center>
<h1>Quickstrom</h1>
<p>High-confidence browser testing</p>
<p>
  <img src="https://github.com/quickstrom-tools/quickstrom/workflows/Test/badge.svg?branch=master" />
</p>
</div>

<hr>

* **Reduce testing effort**

  Generate thousands of test cases instead of writing them manually. Lower
  the maintenance burden of your browser testing by writing concise
  specifications with looser coupling to your implementation.

* **Find complex bugs**

  Quickstrom simulates complex and unexpected user behavior using generative
  random testing. When the specification is violated, Quickstrom finds a
  minimal failing example.

* **Understand your system**

  Focus on specifying your system, not on writing test cases. A specification
  lets you run Quickstrom, but can also increase your team's understanding of
  the system.

* **Adopt gradually**

  Quickstrom works with any web application that renders DOM elements. Start
  simple, and gradually refine your specification to increase coverage and
  confidence.

<hr>

**NOTE:** The Quickstrom project is in a very early phase, so documentation is
scarce. The situation will improve.

## Getting Started


### Prerequisites

* Nix (see [nix.dev](https://nix.dev/) for installation instructions and guides)


### Installation (with Nix)

If you're only looking to run Quickstrom, not hack on it, you can use Cachix
and Nix to get an executable:

```
cachix use quickstrom
nix-build
```

Now, Quickstrom is available in the `result`:

```
result/bin/quickstrom <YOUR SPEC FILE> <ORIGIN URL>
```

Alternatively, install it directly into your environment:

```
nix-env -i -A quickstrom -f default.nix
quickstrom <YOUR SPEC FILE> <ORIGIN URL>
```

**NOTE:** You need to also run geckodriver for Quickstrom to work. See
*[Running](#running) below.

### Starting a Shell

All subsequent commands in the document assume you're in the project's Nix
shell. Either run the following command from the project root:

```
nix-shell
```

Or use [Lorri](https://github.com/target/lorri) and
[direnv](https://direnv.net/) for an automatic and reloading environment
(highly recommended!).

## Running

To check a specification, you must have a running `geckodriver` instance:

```
geckodriver 2>&1 > /dev/null
```

Next, run `quickstrom` and supply the path to the specification file along with
the origin URL (can also be a local file path). Let's run it through Cabal
(assuming you're in a nix-shell):

```
cabal run quickstrom -- /path/to/my/specification http://example.com
```

For instance, you can run the TodoMVC React specification:

```
cabal run quickstrom -- specs/other/TodoMVC.spec.purs http://todomvc.com/examples/react
```

**NOTE:** Running tests can take a lot of time, especially if there's a
failure and Quickstrom tries to shrink to the minimal failing trace.
Optimizations are due, but for now it's _slow_.

### No WebDriver Session

If you get the following error:

```
quickstrom: user error (E NoSession)
```

It's probably because the WebDriver package in Quickstrom failed to clean up
its session. This is a known bug. To work around it, restart Geckodriver:

```
geckodriver 2>&1 > /dev/null
```

And rerun your Quickstrom command.

### Help

There are various flags and options for the `quickstrom` executable. Run with
`--help` to learn more.

## Development

The following instructions are helpful if you want to work on Quickstrom.

### Testing

(Testing Quickstrom itself, that is. Not specifications.)

To run the tests, you need to have a built version of the
[dsl](dsl) package, and set the
`PURESCRIPT_WEBCHECK` environment variable to the package's output
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
WEBCHECK_LIBRARY_DIR=dsl/output cabal test
```


### Formatting

In a Nix shell, run the following command to format all Haskell sources:

```bash
quickstrom-format-sources
```
