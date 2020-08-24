Installation
============

Follow these steps to install Quickstrom locally. For the moment, the only
supported installation method is Nix. Other options might be described in the
future.

Prerequisites
-------------

-  Nix (see `nix.dev <https://nix.dev/>`__ for installation instructions
   and guides)

Installing with Nix
-------------------

To install the ``quickstrom`` executable, use Cachix and Nix to get the
executable:

.. code:: console

   $ cachix use quickstrom
   $ nix-env -iA quickstrom -f https://github.com/quickstrom/quickstrom/tarball/main

Quickstrom should now be available in your environment:

.. code:: console

   $ quickstrom check <YOUR SPEC FILE> <ORIGIN URL>

You need to also run geckodriver for Quickstrom to work. Install it using
Nix:

.. code:: console

   $ nix-env -i geckodriver

You're now ready to `run Quickstrom <#running>`__.
