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
can be changed using the ``--interpreter-log-file`` option.

If you have any syntactic errors or any runtime errors occur when
evaluating the specification, you'll find those in the interpreter
log. In the future, however, we want them to be printed directly by
Quickstrom.
