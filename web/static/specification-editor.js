ace.config.set("basePath", "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.12");

var editor = ace.edit("editor");
editor.setTheme("ace/theme/github");
editor.session.setMode("ace/mode/haskell");
editor.setKeyboardHandler("ace/keyboard/vim");

editor.gotoLine(9, 14);

var form = document.querySelector(".specification-editor");
var checkLog = form.querySelector(".check-log");
var origin = form.querySelector("[name=origin]");

async function scheduleCheck(code, origin) {
  const response = await fetch("/checks", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ code, origin }),
  });
  return response.json();
}

async function readCheck(check) {
  const response = await fetch(check.uri, {
    headers: {
      Accept: "application/json",
    },
  });
  return response.json();
}

function listenEvents(check, callback) {
  return new Promise((resolve, reject) => {
    const src = new EventSource(check.uri + "/events");
    src.addEventListener("message", (msg) => {
      callback(JSON.parse(msg.data));
      if (msg.data.tag === "CheckFinished") {
        src.close();
      }
    });
    src.addEventListener("error", (err) => {
      if (err.eventPhase === src.CLOSED) {
        resolve();
      } else {
        reject(err);
      }
    });
  });
}

function clearCheckLog() {
  checkLog.textContent = "";
}

function appendCheckLog(category, entry) {
  const child = document.createElement("li");
  child.classList.add(category);
  child.textContent = entry;
  checkLog.appendChild(child);
}

function info(msg) {
  return appendCheckLog("info", msg);
}
function warning(msg) {
  return appendCheckLog("warning", msg);
}
function success(msg) {
  return appendCheckLog("success", msg);
}
function failure(msg) {
  return appendCheckLog("failure", msg);
}
function error(msg) {
  return appendCheckLog("error", msg);
}

function logEvent(event) {
  console.log(event);
  switch (event.tag) {
    case "CheckTestEvent":
      switch (event.contents.tag) {
        case "TestStarted":
          return info(
            `Test started (${event.contents.contents.unSize} actions)`
          );
        case "TestPassed":
          return success(
            `Test passed (${event.contents.contents[0].unSize} actions)`
          );
        case "TestFailed":
          return failure(
            `Test failed (${event.contents.contents[0].unSize} actions)`
          );
        case "Shrinking":
          return info(`Shrinking...`);
        case "RunningShrink":
          return info(`Running shrink at level ${event.contents.contents}`);
      }
    case "CheckStarted":
      return info(`Check started, running ${event.contents} tests`);
    case "CheckFinished":
      const checkResult = event.contents;
      switch (checkResult.tag) {
        case "CheckFailure":
          return failure(
            `Failed after ${checkResult.failedAfter} test(s) and ${checkResult.failingTest.numShrinks} shrink(s).`
          );
        case "CheckSuccess":
          return success("All tests passed.");
      }
  }
  console.warn("Unhandled event:", event);
}

form.addEventListener("submit", async (e) => {
  e.preventDefault();
  try {
    const check = await scheduleCheck(editor.getValue(), origin.value);
    clearCheckLog();
    await listenEvents(check, logEvent);
  } catch (e) {
    console.error(e);
  }
});
