ace.config.set("basePath", "/ace");

var editor = ace.edit("editor");
editor.setTheme("ace/theme/github");
editor.session.setMode("ace/mode/haskell");
editor.setKeyboardHandler("ace/keyboard/vim");

var textarea = document.querySelector('.specification-editor textarea[name=spec]');
textarea.style.display = "none";

editor.getSession().setValue(textarea.textContent);
editor.getSession().on('change', function(){
  textarea.textContent = editor.getSession().getValue();
});