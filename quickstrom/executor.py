import dataclasses
import io
import logging
import os
import shutil
import subprocess
import threading
import time
from pathlib import Path
from shutil import which

import png
import selenium.webdriver.chrome.options as chrome_options
import selenium.webdriver.firefox.options as firefox_options
from selenium import webdriver
from selenium.common.exceptions import StaleElementReferenceException
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.firefox.firefox_binary import FirefoxBinary
from selenium.webdriver.remote.webdriver import WebDriver
from selenium.webdriver.remote.webelement import WebElement
from selenium.webdriver.support.select import Select

import quickstrom.printer as printer
import quickstrom.result as result
from quickstrom.hash import dict_hash
from quickstrom.protocol import *

Url = str


@dataclass
class SpecstromError(Exception):
    message: str
    exit_code: int
    log_file: str

    def __str__(self):
        return f"{self.message}, exit code {self.exit_code}"


@dataclass
class SpecstromAbortedError(Exception):
    message: str

    def __str__(self):
        return f"{self.message}"


@dataclass
class PerformActionError(Exception):
    action: Action
    error: Exception

    def __str__(self):
        return f"Error while performing {self.action}:\n\n{self.error}"


@dataclass
class UnsupportedActionError(Exception):
    action: Action

    def __str__(self):
        return f"Unsupported action: {self.action.id}"


@dataclass
class ScriptError(Exception):
    name: str
    script_args: List[JsonLike]
    error: Exception

    def __str__(self):
        return f"Error while invoking script {self.name} with args {self.script_args}:\n{self.error}"


@dataclass
class ClientSideEvents():
    events: List[Action]
    state: State


@dataclass
class Scripts():
    query_state: Callable[[WebDriver, Dict[Selector, Schema]], State]
    install_event_listener: Callable[[WebDriver, Dict[Selector, Schema]], None]
    await_events: Callable[[WebDriver, Dict[Selector, Schema], int],
                           Optional[ClientSideEvents]]


Browser = Union[Literal['chrome'], Literal['firefox']]


@dataclass
class Cookie():
    domain: str
    name: str
    value: str


@dataclass
class CheckBrowser:
    driver: WebDriver
    check: 'Check'

    def close(self):
        try:
            self.driver.quit()
        finally:
            if self.check.browser == "chrome" and self.check.browser_log_file and self.check.browser_data_directory:
                self.check.log.info("Copying browser log file to: %s", self.check.browser_log_file)
                # copy file from user data directory to browser log file:
                log_file = Path(self.check.browser_data_directory, "Default", "chrome_debug.log")
                shutil.copyfile(log_file, self.check.browser_log_file)


@dataclass
class Check():
    module: str
    origin: str
    browser: Browser
    browser_binary: Optional[str]
    include_paths: List[str]
    headless: bool
    capture_screenshots: bool
    cookies: List[Cookie]
    driver_log_file: Optional[str]
    browser_log_file: Optional[str]
    browser_data_directory: Optional[str]
    interpreter_log_file: IO
    log: logging.Logger = logging.getLogger('quickstrom.executor')

    def execute(self) -> List[result.PlainResult]:
        scripts = self.load_scripts()

        with self.launch_specstrom(self.interpreter_log_file) as p:
            assert p.stdout is not None
            assert p.stdin is not None
            input_messages = message_reader(p.stdout)
            output_messages = message_writer(p.stdin)
            screenshots: Dict[str, result.Screenshot[bytes]] = {}

            def receive():
                msg = input_messages.read()
                exit_code = p.poll()
                if msg is None and exit_code is not None:
                    if exit_code == 0:
                        return None
                    else:
                        raise SpecstromError("Specstrom invocation failed",
                                             exit_code,
                                             self.interpreter_log_file.name)
                else:
                    self.log.debug("Received %s", msg)
                    return msg

            def send(msg):
                exit_code = p.poll()
                if exit_code is None:
                    self.log.debug("Sending %s", msg)
                    output_messages.write(msg)
                elif exit_code == 0:
                    self.log.warning("Done, can't send.")
                else:
                    self.log.warning("Specstrom errored, can't send.")

            def perform_action(driver, action):
                try:
                    if action.id == 'noop':
                        pass
                    elif action.id == 'click':
                        id = action.args[0]
                        element = WebElement(driver, id)
                        ActionChains(driver).move_to_element(element).click(
                            element).perform()
                    elif action.id == 'doubleClick':
                        id = action.args[0]
                        element = WebElement(driver, id)
                        ActionChains(driver).move_to_element(
                            element).double_click(element).perform()
                    elif action.id == 'select':
                        id = action.args[0]
                        value = action.args[1]
                        option = WebElement(driver, id)
                        select = Select(
                            option.find_element(By.XPATH,
                                                "./ancestor::select"))
                        select.select_by_value(value)
                    elif action.id == 'focus':
                        id = action.args[0]
                        element = WebElement(driver, id)
                        element.send_keys("")
                    elif action.id == 'keyPress':
                        char = action.args[0]
                        element = driver.switch_to.active_element
                        element.send_keys(char)
                    elif action.id == 'enterText':
                        element = driver.switch_to.active_element
                        element.send_keys(action.args[0])
                    elif action.id == 'enterTextInto':
                        id = action.args[1]
                        element = WebElement(driver, id)
                        element.send_keys(action.args[0])
                    elif action.id == 'clear':
                        id = action.args[0]
                        element = WebElement(driver, id)
                        element.clear()
                    elif action.id == 'scrollBy':
                        driver.execute_script(
                            "window.scrollBy(arguments[0], arguments[1])",
                            action.args[0], action.args[1])
                    else:
                        raise UnsupportedActionError(action)
                except Exception as e:
                    raise PerformActionError(action, e)

            def screenshot(driver: WebDriver, hash: str):
                if self.capture_screenshots:
                    bs: bytes = driver.get_screenshot_as_png(
                    )  # type: ignore
                    (width, height, _, _) = png.Reader(io.BytesIO(bs)).read()
                    window_size = driver.get_window_size()
                    scale = round(width / window_size['width'])
                    if scale != round(height / window_size['height']):
                        self.log.warning(
                            "Width and height scales do not match for screenshot"
                        )
                    screenshots[hash] = result.Screenshot(image=bs,
                                                          width=width,
                                                          height=height,
                                                          scale=scale)

            def attach_screenshots(
                    r: result.PlainResult) -> result.PlainResult:
                def on_state(state):
                    return result.State(screenshot=screenshots.get(
                        state.hash, None),
                        queries=state.queries,
                        hash=state.hash)

                return result.map_states(r, on_state)

            def await_events(driver, deps, state_version, timeout: int):
                def on_no_events():
                    state = scripts.query_state(driver, deps)
                    screenshot(driver, dict_hash(state))
                    state_version.increment()
                    send(Timeout(state=state))

                try:
                    self.log.debug(f"Awaiting events with timeout {timeout}")
                    events = scripts.await_events(driver, deps, timeout)
                    self.log.debug(f"Change: {events}")

                    if events is None:
                        self.log.info(f"Timed out!")
                        on_no_events()
                    else:
                        screenshot(driver, dict_hash(events.state))
                        state_version.increment()
                        send(Events(events.events, events.state))
                except StaleElementReferenceException as e:
                    self.log.error(f"Stale element reference: {e}")
                    on_no_events()

            def run_sessions() -> List[result.PlainResult]:
                while True:
                    msg = receive()
                    assert msg is not None
                    if isinstance(msg, Start):
                        try:
                            self.log.info("Starting session")
                            browser = self.new_browser()
                            browser.driver.set_window_size(1200, 1200)

                            if len(self.cookies) > 0:
                                # First we need to visit the page in order to set cookies.
                                browser.driver.get(self.origin)
                                for cookie in self.cookies:
                                    self.log.debug(f"Setting {cookie}")
                                    browser.driver.add_cookie(
                                        dataclasses.asdict(cookie))
                            # Now that cookies are set, we have to visit the origin again.
                            browser.driver.get(self.origin)
                            # Hacky sleep to allow page load.
                            time.sleep(1)

                            state_version = Counter(initial_value=0)

                            scripts.install_event_listener(
                                browser.driver, msg.dependencies)
                            await_events(browser.driver, msg.dependencies,
                                         state_version, 10000)

                            await_session_commands(browser, msg.dependencies,
                                                   state_version)
                        except SpecstromAbortedError as e:
                            raise e
                        except Exception as e:
                            send(Error(str(e)))
                            msg = receive()
                            if not isinstance(msg, End):
                                raise Exception(
                                    f"Expected End after Error but got: {msg}")
                    elif isinstance(msg, Done):
                        return [
                            attach_screenshots(result.from_protocol_result(r))
                            for r in msg.results
                        ]
                    elif isinstance(msg, Aborted):
                        raise SpecstromAbortedError(msg.error_message)
                    else:
                        raise Exception(
                            f"Unexpected message in run_sessions: {msg}")

            def await_session_commands(browser: CheckBrowser, deps, state_version):
                try:
                    while True:
                        msg = receive()

                        if not msg:
                            raise Exception(
                                "No more messages from Specstrom, expected RequestAction or End."
                            )
                        elif isinstance(msg, RequestAction):
                            if msg.version == state_version.value:
                                self.log.info(
                                    f"Performing action in state {state_version.value}: {printer.pretty_print_action(msg.action)}"
                                )

                                perform_action(browser.driver, msg.action)

                                if msg.action.timeout is not None:
                                    self.log.debug(
                                        "Installing change observer")
                                    scripts.install_event_listener(
                                        browser.driver, deps)

                                state = scripts.query_state(browser.driver, deps)
                                screenshot(browser.driver, dict_hash(state))
                                state_version.increment()
                                send(Performed(state=state))

                                if msg.action.timeout is not None:
                                    await_events(browser.driver, deps, state_version,
                                                 msg.action.timeout)
                            else:
                                self.log.warning(
                                    f"Got stale message ({msg}) in state {state_version.value}"
                                )
                                send(Stale())
                        elif isinstance(msg, AwaitEvents):
                            if msg.version == state_version.value:
                                self.log.info(
                                    f"Awaiting events in state {state_version.value} with timeout {msg.await_timeout}"
                                )
                                scripts.install_event_listener(browser.driver, deps)
                                await_events(browser.driver, deps, state_version,
                                             msg.await_timeout)
                            else:
                                self.log.warning(
                                    f"Got stale message ({msg}) in state {state_version.value}"
                                )
                                send(Stale())
                        elif isinstance(msg, End):
                            self.log.info("Ending session")
                            return
                        elif isinstance(msg, Aborted):
                            raise SpecstromAbortedError(msg.error_message)
                        else:
                            raise Exception(f"Unexpected message: {msg}")
                finally:
                    browser.close()

            return run_sessions()

    def launch_specstrom(self, ilog):
        includes = list(map(lambda i: "-I" + i, self.include_paths))
        cmd = ["specstrom", "check", self.module
               ] + includes  # + ["+RTS", "-p"]
        self.log.debug("Invoking Specstrom with: %s", " ".join(cmd))
        return subprocess.Popen(cmd,
                                text=True,
                                stdout=subprocess.PIPE,
                                stderr=ilog,
                                stdin=subprocess.PIPE,
                                bufsize=0)

    def new_browser(self) -> CheckBrowser:
        if self.browser == 'chrome':
            options = chrome_options.Options()
            options.headless = self.headless
            browser_path = self.browser_binary or which("chromium") or which(
                "google-chrome-stable") or which("google-chrome") or which(
                "chrome")
            options.binary_location = browser_path  # type: ignore
            options.add_argument('--no-sandbox')
            options.add_argument("--single-process")
            if self.browser_log_file:
                if not self.browser_data_directory:
                    raise Exception("--browser-log-file requires --browser-data-directory to be set")
                options.add_argument("--enable-logging")
                options.add_argument("--v=1")
                options.add_argument(f"--user-data-dir={self.browser_data_directory}")
            chromedriver_path = which('chromedriver')

            if not chromedriver_path:
                raise Exception("chromedriver not found in PATH")

            return CheckBrowser(check=self,
                                driver=webdriver.Chrome(options=options,
                                                        executable_path=chromedriver_path,
                                                        service_log_path=self.driver_log_file))
        elif self.browser == 'firefox':
            options = firefox_options.Options()
            options.headless = self.headless
            binary = FirefoxBinary(self.browser_binary or which("firefox"))
            geckodriver_path = which('geckodriver')
            if not geckodriver_path:
                raise Exception("geckodriver not found in PATH")
            if self.driver_log_file:
                options.log.level = "debug"
            if self.browser_log_file:
                options.add_argument("--MOZ_LOG=timestamp,nsHttp:3,cache2:3,nsSocketTransport:3,nsHostResolver:3,cookie:3")
                options.add_argument(f"--MOZ_LOG_FILE={self.browser_log_file}")
            if self.browser_data_directory:
                # `--profile` causes Firefox to hang and not display the origin page for some reason, so it's disabled
                # for now
                raise Exception("--browser-data-directory is not supported for Firefox")

            return CheckBrowser(check=self,
                                driver=webdriver.Firefox(options=options,
                                                         firefox_binary=binary,
                                                         executable_path=geckodriver_path,
                                                         service_log_path=self.driver_log_file))
        else:
            raise Exception(f"Unsupported browser: {self.browser}")

    def load_scripts(self) -> Scripts:
        def map_query_state(r):
            if r is None:
                raise Exception(
                    "WebDriver script invocation failed with unexpected None result. This might be caused by an unexpected page navigation in the browser. Consider adding a timeout to the corresponding action."
                )
            return elements_to_refs(r)

        def map_client_side_events(r):
            def map_event(e: dict):
                if e['tag'] == 'loaded':
                    return Action(id='loaded',
                                  args=[],
                                  isEvent=True,
                                  timeout=None)
                elif e['tag'] == 'changed':
                    return Action(id='changed',
                                  args=[elements_to_refs(e['element'])],
                                  isEvent=True,
                                  timeout=None)
                elif e['tag'] == 'detached':
                    return Action(id='detached',
                                  args=[e['markup']],
                                  isEvent=True,
                                  timeout=None)
                else:
                    raise Exception(f"Invalid event tag in: {e}")

            return ClientSideEvents([map_event(e) for e in r['events']],
                                    elements_to_refs(
                                        r['state'])) if r is not None else None

        result_mappers = {
            'queryState': map_query_state,
            'installEventListener': lambda r: r,
            'awaitEvents': map_client_side_events,
        }

        def load_script(name: str, is_async: bool = False) -> Any:
            key = 'QUICKSTROM_CLIENT_SIDE_DIRECTORY'
            client_side_dir = os.getenv(key)
            if not client_side_dir:
                raise Exception(f'Environment variable {key} must be set')
            file = open(f'{client_side_dir}/{name}.js')
            script = file.read()

            def f(driver: WebDriver, *args: Any) -> JsonLike:
                try:
                    r = driver.execute_async_script(
                        script, *args) if is_async else driver.execute_script(
                        script, *args)
                    return result_mappers[name](r)
                except StaleElementReferenceException as e:
                    raise e
                except Exception as e:
                    raise ScriptError(name, list(args), e)

            return f

        return Scripts(
            query_state=load_script('queryState'),
            install_event_listener=load_script('installEventListener'),
            await_events=load_script('awaitEvents', is_async=True),
        )


def elements_to_refs(obj: Any) -> Any:
    if isinstance(obj, dict):
        return {key: elements_to_refs(value) for (key, value) in obj.items()}
    elif isinstance(obj, list):
        return [elements_to_refs(value) for value in obj]
    elif isinstance(obj, WebElement):
        return obj.id
    else:
        return obj


class Counter(object):
    def __init__(self, initial_value=0):
        self.value = initial_value
        self._lock = threading.Lock()

    def increment(self):
        with self._lock:
            self.value += 1
