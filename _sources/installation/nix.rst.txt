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

Quickstrom should now be available in your environment:

.. code-block:: console

   $ quickstrom check <YOUR SPEC FILE> <ORIGIN URL>

You need to also run geckodriver for Quickstrom to work. Install it using
Nix:

.. code-block:: console

   $ nix-env -i geckodriver

You're now ready to :doc:`run Quickstrom <../running>`.
