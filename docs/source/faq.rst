FAQ
===

Here are some frequently asked questions about Quickstrom:

Isn’t this just property-based testing for web applications?
------------------------------------------------------------

Well, not exactly. Quickstrom definitely is a form of property-based
testing (PBT), but it’s not *only* that. Being specifically designed for
testing web applications, Quickstrom can reduce the amount of work you
need to do in order to test properties of your system:

-  Quickstrom discovers and performs actions automatically
-  You specify only the properties you care about, and you don’t have to
   write a fully functional model
-  Quickstrom aims to (in the future) perform fault injection
   automatically, such as delaying, cancelling, or manipulating XHR
   responses, run in concurrent tabs, manipulate cookies or web storage,
   etc

It might be useful to think of Quickstrom as a mix of PBT, black-box
browser testing, and a specification system like TLA+. One aim is “to be
the `Jepsen <http://jepsen.io/>`__ for web applications.”
