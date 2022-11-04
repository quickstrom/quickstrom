Reporters
=========

After a Quickstrom check completes, one or more *reporters* run. They report
the result of the check in different formats. The following reporters are
available:

* ``console``
* ``html``
* ``json``

Invoke reporters by passing the ``--reporter=<NAME>`` option to the ``check``
command.

Console
-------

The console reporter is invoked by default. It prints a trace and summary to
the console when a check fails. The trace contains information about the state
of queried elements at each state in the behavior, along with the actions taken
by Quickstrom.

HTML
----

The HTML reporter creates a report for web browsers in a given directory. The
report is an interactive troubleshooting tool based on state transitions,
showing screenshots and overlayed state information for the queried elements.

To set the directory to generate the report in, use the option
``--html-report-directory=<DIR>``.

The HTML report directory must be served through an HTTP server in order
to avoid problems with CORS. If you have Python 3 installed, serve it with
the following command:

.. code-block:: console

   $ python3 -m http.server -d <DIR>

JSON
----

The JSON report works similarly to the HTML report, except it generates only a
JSON file and screenshots, no HTML files. In the report directory you'll find
a file ``report.json`` that you can work with.

To set the directory to generate the report in, use the option
``--json-report-directory=<DIR>``.