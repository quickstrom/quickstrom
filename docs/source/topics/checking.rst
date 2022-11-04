Checking
========

To check a web application against a specification, use the
``quickstrom check`` command. Supply the module name of the
specification along with the origin URL.

.. code-block:: console

   $ quickstrom check \
      spec-module-name \
      http://example.com

The origin can also be a local file:

.. code-block:: console

   $ quickstrom check \
      spec-module-name \
      /path/to/my/webapp.html

Cross-Browser Testing
---------------------

Quickstrom currently supports these browsers:

- Firefox (``firefox``)
- Chrome/Chromium (``chrome``)

Unless specified, the default browser used is Firefox. To override,
use the ``--browser`` option and set the appropriate browser when
running the ``check`` command:


.. code-block:: console

   $ quickstrom check \
      --browser=chrome \
      ... # more options
