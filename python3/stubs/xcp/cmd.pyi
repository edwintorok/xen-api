from basedtyping import Untyped
from typing import Any
from xcp import logger as logger
from xcp.compat import open_defaults_for_utf8_text as open_defaults_for_utf8_text

def runCmd(command: bytes | str | list[str], with_stdout: bool = False, with_stderr: bool = False, inputtext: bytes | str | None = None, **kwargs: Any) -> Any: ...

class OutputCache:
    cache: Untyped
    def __init__(self): ...
    def fileContents(self, fn, *args, **kwargs) -> Untyped: ...
    def runCmd(self, command, with_stdout: bool = False, with_stderr: bool = False, inputtext: Untyped | None = None, **kwargs) -> Untyped: ...
    def clearCache(self): ...
