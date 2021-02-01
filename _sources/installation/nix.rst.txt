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
   $ nix-env -iA quickstrom -f https://github.com/quickstrom/quickstrom/tarball/main

Verify that Quickstrom is now available in your environment:

.. code-block:: console

   $ quickstrom version

You need to run a WebDriver server for Quickstrom checks to work. This
user documentation mostly uses GeckoDriver and Firefox, but you can
use other browsers and WebDriver servers.

Install GeckoDriver using Nix:

.. code-block:: console

   $ nix-env -i geckodriver

You're now ready to :doc:`check webapps using Quickstrom <../topics/checking>`.
