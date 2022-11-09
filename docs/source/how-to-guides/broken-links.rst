Broken Links
============

This is simple specification used for finding broken internal links. External
links are not followed. It's based on Domen Kožar's gist. [#original]_

.. literalinclude:: broken-links.strom
   :language: javascript


Tweak the patterns and the predicate to fit your use case. You might not have
status codes rendered, but texts like "Page not found" or "Access denied".

.. [#original] Domen Kožar wrote the original specification for `Cachix <https://cachix.org>`__ using one of the first versions of Quickstrom, and published it along with a GitHub Actions setup: https://gist.github.com/domenkozar/71135bf7aa6d50d6911fb74f4dcb4bad
