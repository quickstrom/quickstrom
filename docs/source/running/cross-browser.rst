Cross-Browser Testing
=====================

Quickstrom currently supports these browsers:

- Firefox (``firefox``)
- Chrome/Chromium (``chrome``)

Set the ``--browser`` option to the appropriate browser when running the
``check`` command:


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