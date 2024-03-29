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

       # We use `install-nix-action` and `cachix-action` to quickly install
       # Quickstrom from a binary cache.
       - uses: cachix/install-nix-action@v18
       - uses: cachix/cachix-action@v12
         with:
           name: quickstrom
       - run: nix-env -iA quickstrom -f https://github.com/quickstrom/quickstrom/tarball/{VERSION}

       # Now, run tests! This assumes there's a file called 
       # `example.strom` in the root of the GitHub repository.
       - run: quickstrom check example https://example.com --reporter=html --html-report-directory=report

       # Finally, we archive HTML report as an artifact. This
       # can be downloaded an inspected after failed checks.
       - name: Archive test results
         uses: actions/upload-artifact@v3
         with:
           name: test-report
           path: report

Replace the placeholder paths and URLs. 

Next Steps
----------

* You might want to run tests in Chrome instead. See :doc:`../topics/checking` for instructions on using other browsers.
* If you'd like to check multiple specs and in multiple browsers, see `matrix configurations <https://docs.github.com/en/actions/reference/workflow-syntax-for-github-actions#jobsjob_idstrategymatrix>`_ in the GitHub Actions documentation.

.. [#original] Domen Kožar wrote the original GitHub Action configuration for `Cachix <https://cachix.org>`__: https://gist.github.com/domenkozar/71135bf7aa6d50d6911fb74f4dcb4bad
