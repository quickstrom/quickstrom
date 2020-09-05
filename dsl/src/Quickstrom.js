/**
 * NOTE: This file is only stubs, and only used to please the
 * PureScript compiler. The code in this file isn't run by Quickstrom,
 * and shouldn't be run anywhere else.
 */
"use strict";

exports.next = function(x) { return x; };
exports.always = function(x) { return true; };
exports.trace = function (t) { return function(x) { console.log(t); return x; } };
exports._queryAll = function(selector) { return function (states) { return queriedElements[selector] || []; } };
exports._property = function(name) { return { tag: "property", name: name }; };
exports._attribute = function(name) { return { tag: "attribute", name: name }; };
exports.cssValue = function(name) { return { tag: "cssValue", name: name }; };
