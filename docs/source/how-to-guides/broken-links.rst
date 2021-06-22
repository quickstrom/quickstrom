Broken Links
============

This is simple specification used for finding broken internal links. External
links are not followed. It's based on Domen Kožar's gist. [#original]_

.. literalinclude:: BrokenLinks.spec.purs
   :language: haskell


Tweak the patterns and the precicate to fit your use case. You might not have
status codes rendered, but texts like "Page not found".

.. [#original] Domen Kožar wrote the original specification for `Cachix <https://cachix.org>`__ and published it along with a GitHub Actions setup: https://gist.github.com/domenkozar/71135bf7aa6d50d6911fb74f4dcb4bad