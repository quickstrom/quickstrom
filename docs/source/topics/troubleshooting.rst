Troubleshooting
===============

This documents collects some common problems and tips on how to
identify what's not working correctly.

If you're troubleshooting a failing Quickstrom check, make sure to
enable debug logs:

.. code-block:: console

   $ quickstrom --log-level=DEBUG check ...

Also, the underlying Specstrom interpreter outputs its log file in the
current working directory, called ``interpreter.log``. Its location
can be changed using the ``--interpreter-log-file=...`` option.

Finally, the Webdriver log can be stored by specifying ``--driver-log-file=...``.
