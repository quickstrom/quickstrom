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
- Remote (``remote``)

Unless specified, the default browser used is Firefox. To override,
use the ``--browser`` option and set the appropriate browser when
running the ``check`` command:


.. code-block:: console

   $ quickstrom check \
      --browser=chrome \
      ... # more options


You can also override which binary it uses when launching the browser:

.. code-block:: console

   $ quickstrom check \
      --browser=chrome \
      --browser-binary=/usr/bin/google-chrome-stable \
      ... # more options

The ``remote`` browser mode is useful for services like SauceLabs or BrowserStack, where you pass in the desired
capabilities to control the environment:

.. code-block:: console

   $ capabilities=$(cat <<-END
     {
       "browserName": "Chrome",
       "browserVersion": "102.0",
       ...
     }
     END
   )
   $ quickstrom check \
      --browser=remote \
       --remote-webdriver-url="https://webdriver.example.com:443/wd/hub" \
       --remote-desired-capabilities="$capabilities" \
      ... # more options
