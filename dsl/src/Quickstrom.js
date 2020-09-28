"use strict";

exports._next = function(x) { return x; };
exports._always = function(x) { return true; };
exports._until = function(x) { function(y) { return true; }; };
exports._trace = function (t) { return function(x) { console.log(t); return x; } };
exports._queryAll = function(selector) { return function (states) { return queriedElements[selector] || []; } };
exports._property = function(name) { return { tag: "property", name: name }; };
exports._attribute = function(name) { return { tag: "attribute", name: name }; };
exports.cssValue = function(name) { return { tag: "cssValue", name: name }; };
