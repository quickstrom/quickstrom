Installing with Nix
===================

Follow these steps to install Quickstrom using Nix.

Prerequisites
-------------

-  Nix (see `nix.dev <https://nix.dev/>`__ for installation instructions
   and guides)

Installing with Nix
-------------------

To install the ``quickstrom`` executable, use Cachix and Nix to get the
executable:

.. code-block:: console

   $ cachix use quickstrom
   $ nix-env -iA quickstrom -f https://github.com/quickstrom/pyquickstrom/tarball/main

If you're on Darwin, you're probably going to have problems with
Firefox and Chrome from nixpkgs. You can exclude browsers from the
Quickstrom environment and provide them on your own, but there's often
problems with version mismatches between chromedriver and
Chrome/Chromium. If you want to do this, override it with this command:

.. code-block:: console

   $ nix-env -iA quickstrom -f https://github.com/quickstrom/pyquickstrom/tarball/main \
       --arg includeBrowsers false

Verify that Quickstrom is now available in your environment:

.. code-block:: console

   $ quickstrom --help

You're now ready to :doc:`check webapps using Quickstrom <../topics/checking>`.
