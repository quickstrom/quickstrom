# TodoMVCs TODO
https://github.com/tastejs/todomvc-app-template/commit/2055b9d76475336324d01cd825f6462763ec3c19

## Results

✓ examples/backbone 
❌ angularjs
    - inconsistent clearing of input field
✓ emberjs
✓ knockoutjs 
? dojo 
✓ knockback 
✓ canjs 
✓ polymer/index.html 
✓ react 
❌ mithril 
    - inconsistent clearing of input field
✓ vue 
✓ backbone_marionette 
✓ kotlin-react 
✓ spine 
✓ vanilladart/build/web 
✓ gwt 
✓ closure 
✓ elm 
? angular-dart/web 
     - ???
✓ typescript-backbone 
✓ typescript-angular 
✓ typescript-react 
✓ reagent 
✓ scalajs-react 
✓ binding-scala 
✓ js_of_ocaml 
❌ humble 
    - 404
✓ backbone_require 
❌ knockoutjs_require 
    - renders an empty list items and "0 left", quick glitch, then renders the regular initial state
✓ angularjs_require 
    - Had to await ".ng-scope", as template strings were present (but hidden) in the DOM before
✓ canjs_require 
❌ lavaca_require 
    - inconsistent clearing of input field (same as Angular)
❌ cujo/index.html 
    - no filters
    - race condition in initialization: focus input and press Return before event listeners are 
      attached, results in `index.html?text=`
✓ sammyjs 
❌ somajs 
    - 404
❌ duel/www 
    - inconsistent clearing of input field (same as Angular)
✓ kendo 
❌ dijon 
    - no filters
✓ enyo_backbone 
❌ sapui5 
    - no input field
✓ exoskeleton 
✓ ractive 
✓ react-alt 
✓ react-backbone 
✓ aurelia 
❌ angular2 
    - no filters
✓ riotjs 
✓ jsblocks

### Real-time

❌ http://todomvc-socketstream.herokuapp.com/
    - Inconclusive: can't clear state between runs
✓ http://todomvc.com/examples/firebase-angular

### Node.js

❌ http://gcloud-todos.appspot.com/
    - 404

### Non-framework implementation

❌ vanillajs
    - adds pending item on other iteraction (toggle all, change filter)
❌ vanilla-es6
    - minor: `.todo-count strong` is missing
    - same as `vanillajs`: adds pending item on other iteraction
✓ jquery

### Other sources

✓ https://swannodette.github.io/todomvc/labs/architecture-examples/om/