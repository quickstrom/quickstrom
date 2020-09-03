Checking
========

This guide assumes you've installed Quickstrom and a compatible WebDriver
server, as described in one of the :doc:`installation guides
<../installation>`. If you haven't done that already, start there.

To check a specification, you must have a running WebDriver server. In the
rest of this guide we'll use GeckoDriver and Firefox. Learn more about using
other browsers and WebDriver servers in :doc:`cross-browser`.

Run the following command in a separate terminal or tab:

.. code-block:: console

   $ geckodriver

.. note::

   The invocation of geckodriver might look different depending on which
   installation method you've used.

Next, run ``quickstrom check`` and supply the path to the specification
file along with the origin URL (can also be a local file path).

.. code-block:: console

   $ quickstrom check \
      --browser=firefox \
      /path/to/my/specification \
      http://example.com