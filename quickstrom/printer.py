from selenium.webdriver.common.keys import Keys
import quickstrom.protocol as protocol

key_name_by_code = {code: key for key, code in vars(Keys).items()}

def pretty_print_action(action: protocol.Action) -> str:
    def format_arg(arg):
        return key_name_by_code.get(
            arg, repr(arg)) if action.id == 'keyPress' else repr(arg)

    timeout_suffix = f" timeout {action.timeout}" if action.timeout is not None else ""

    return f"{action.id}({', '.join([format_arg(arg) for arg in action.args])}){timeout_suffix}"


