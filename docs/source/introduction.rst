Introduction
============

By using Quickstrom, you will:

-  **Reduce testing effort**

   Generate thousands of test cases instead of writing them manually.
   Lower the maintenance burden of your browser testing by writing
   concise specifications with looser coupling to your implementation.

-  **Find complex bugs**

   Quickstrom simulates complex and unexpected user behavior using
   generative random testing. When the specification is violated,
   Quickstrom finds a minimal failing example.

-  **Understand your system**

   Focus on specifying your system, not on writing test cases. A
   specification lets you run Quickstrom, but can also increase your
   team’s understanding of the system.

-  **Adopt gradually**

   Quickstrom works with any web application that renders DOM elements.
   Start simple, and gradually refine your specification to increase
   coverage and confidence.

That sounds cool, but does it actually work?

Yes! Check out `The TodoMVC
Showdown <https://wickstrom.tech/programming/2020/07/02/the-todomvc-showdown-testing-with-webcheck.html>`__
case study and learn how Quickstrom (previously called *WebCheck*) found
problems in multiple implementations of TodoMVC.

.. toctree::
   :hidden:

   introduction/how-it-works
   introduction/specification-language
   introduction/example
   introduction/next-steps