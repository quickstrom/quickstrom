FAQ
===

Here are some frequently asked questions about Quickstrom:

Isn’t this just property-based testing for web applications?
------------------------------------------------------------

Quickstrom definitely is a form of property-based testing (PBT), but
it’s not *only* that. Being specifically designed for testing web
applications, Quickstrom can reduce the amount of work you need to do
in order to test properties of your system:

- Quickstrom discovers and performs actions automatically
- You specify only the properties you care about, and you don’t have
  to write a fully functional model
- Quickstrom aims to (in the future) perform fault injection
  automatically, such as delaying, cancelling, or manipulating XHR
  responses, run in concurrent tabs, manipulate cookies or web storage,
  etc

It might be useful to think of Quickstrom as a mix of PBT, black-box
browser testing, and a specification system like TLA+. One aim is “to be
the `Jepsen <http://jepsen.io/>`__ for web applications.”

Why should I use Quickstrom instead of a model-based property test?
-------------------------------------------------------------------

You might argue that this is just property-based testing, and that you could
do this with state machine testing. And you’d be right! Similar tests could
be written using a state machine model, WebDriver, and property-based
testing.

With Quickstrom, however, you don’t have to write a model that fully
specifies the behavior of your system. Instead, you describe the most
important state transitions and leave the rest unspecified. You can
gradually adopt Quickstrom and improve your specifications over time.

Furthermore, in problem domains where there’s lots of of *essential
complexity*, models tend to become as complex. For example, it’s often hard
to find a naive implementation for your model when your modelling a business
system with a myriad of arbitrary rules.

Finally, by using linear temporal logic, we can express safety and
liveness properties in specifications. This is something that you'd
have to build yourself on top of regular property tests.
