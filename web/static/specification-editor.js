ace.config.set("basePath", "/ace");

var editor = ace.edit("editor");
editor.setTheme("ace/theme/github");
editor.session.setMode("ace/mode/haskell");
editor.setKeyboardHandler("ace/keyboard/vim");


editor.gotoLine(9, 14);
editor.focus();

var form = document.querySelector(".specification-editor");
var submit = form.querySelector("[type=submit]");
var origin = form.querySelector("[name=origin]");

form.addEventListener("submit", (e) => {
  e.preventDefault();
});
