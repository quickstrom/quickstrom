Testing in GitHub Actions
=========================

Quickstrom can be run in a continuous integration (CI) workflow to find problems
early. Here's a configuration for GitHub Actions that checks a website on every
commit to the ``main`` branch. It's based on Domen Kožar's gist. [#original]_

.. code-block:: yaml

   name: "Quickstrom integration tests"
   on:
     push:
       branches: 
         - main
   jobs:
     build:
       runs-on: ubuntu-latest
       steps:
       - uses: actions/checkout@v2.3.4

       # We use `install-nix-action` and `cachix-action` to quickly install the 
       # latest Quickstrom from a binary cache.
       - uses: cachix/install-nix-action@v12
       - uses: cachix/cachix-action@v8
         with:
           name: quickstrom
       - run: nix-env -iA quickstrom -f https://github.com/quickstrom/quickstrom/tarball/main

       # We install and run Geckodriver in the background, so that we can run
       # tests using Firefox.
       - run: nix-env -i geckodriver -f https://github.com/NixOS/nixpkgs/tarball/nixos-21.05
       - run: geckodriver&

       # Finally, run tests! This assumes there's a file called 
       # `example.spec.purs` in the root of the GitHub repository.
       - run: quickstrom check example.spec.purs https://example.com

Replace the placeholder paths and URLs. 

Next Steps
----------

* You might want to run tests in Chrome instead. See :doc:`../topics/checking` for instructions on using other browsers.
* If you'd like to check multiple specs and in multiple browsers, see `matrix configurations <https://docs.github.com/en/actions/reference/workflow-syntax-for-github-actions#jobsjob_idstrategymatrix>`_ in the GitHub Actions documentation.

.. [#original] Domen Kožar wrote the original GitHub Action configuration for `Cachix <https://cachix.org>`__: https://gist.github.com/domenkozar/71135bf7aa6d50d6911fb74f4dcb4bad