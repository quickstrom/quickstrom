Checking
========

To check a web application against a specification, use the
``quickstrom check`` command. Supply the path to the specification
file along with the origin URL.

.. code-block:: console

   $ quickstrom check \
      /path/to/my/specification \
      http://example.com

The origin can also be a local file:

.. code-block:: console

   $ quickstrom check \
      /path/to/my/specification \
      /path/to/my/webapp.html

.. note::

   To check a specification, you must have a running WebDriver
   server. Most guides in this user documentation use GeckoDriver and
   Firefox. Other options are discussed below.

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

If you need to specify the executable, use ``--browser-binary``:

.. code-block:: console

   $ quickstrom check \
      --browser=chrome \
      --browser-binary=/path/to/google-chrome \
      ... # more options

WebDriver Options
-----------------

If your WebDriver server is running on a different host, port, or path than
the default (``http://127.0.0.1:4444``), you can override those options:

.. code-block:: console

   $ quickstrom check \
      --webdriver-host=hub.example.com \
      --webdriver-port=12345 \
      --webdriver-path="/wd/hub" \
      ... # more options
