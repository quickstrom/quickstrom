from abc import abstractmethod
from dataclasses import dataclass
import quickstrom.result as result

@dataclass
class Reporter():
    @abstractmethod
    def report(self, result: result.PlainResult) -> None:
        pass
