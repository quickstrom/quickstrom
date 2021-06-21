Broken Links
============

This is simple specification used for finding broken internal links. External
links are not followed. It's based on Domen Kožar's gist. [#original]_

.. code-block:: haskell

   module BrokenLinks where
   
   import Quickstrom
   import Data.Foldable (any)
   import Data.Maybe (maybe)
   import Data.Tuple (Tuple(..))
   import Data.String.CodeUnits (contains)
   import Data.String.Pattern (Pattern(..))
   
   readyWhen = "body"
   
   actions :: Actions
   actions = [
     -- Only links beginning with a slash are followed. We 
     -- could also use an absolute base URL, e.g:
     --
     --     click "a[href^='https://example.com']"
     --
     click "a[href^='/']"
   ]
   
   -- We're interested in finding links that lead to pages 
   -- rendered with these status codes in the heading.
   patterns = map Pattern [ "404", "500" ]
   
   -- In our system's error pages, status codes are rendered 
   -- in an <h1> element.
   heading = 
     maybe "" _.textContent (queryOne "h1" { textContent })
   
   -- Check if the heading has any of the error codes in the 
   -- text.
   hasErrorCode = 
     any (\p -> contains p heading) patterns
   
   -- This is the safety property. At no point, following 
   -- only internal links, should we see error codes.
   proposition = always (not hasErrorCode)

Tweak the patterns and the precicate to fit your use case. You might not have
status codes rendered, but texts like "Page not found".

.. [#original] Domen Kožar wrote the original specification for `Cachix <https://cachix.org>`__ and published it along with a GitHub Actions setup: https://gist.github.com/domenkozar/71135bf7aa6d50d6911fb74f4dcb4bad