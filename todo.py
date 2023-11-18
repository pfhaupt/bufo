import subprocess
from typing import List
import random

def call_cmd(cmd: List) -> int:
    return subprocess.check_output(cmd).split(b'\n')

def get_random():
    todo = call_cmd(["git", "grep", "-nE", "(todo!|TODO|FIXME)", "--", ":!todo.py"])
    print(f"Found {len(todo)} things that need fixing!")
    if len(todo) == 0:
        return "DONE! No todo!(), TODO or FIXME found!"
    return str(random.choice(todo))

print(get_random())