import subprocess
from typing import List
import random

from sys import argv

CMD = ["git", "grep", "-nE", "(todo!|TODO|FIXME|REVIEW)", "--", ":!todo.py"]

def call_cmd(cmd: List) -> int:
    return subprocess.check_output(cmd).split(b'\n')

def get_random():
    todo = call_cmd(CMD)
    todo = list(filter(lambda elem: elem != b'', todo))
    print(f"Found {len(todo)} things that need fixing!")
    if len(todo) == 0:
        return "DONE! No todo!(), TODO or FIXME found!"
    return str(random.choice(todo))

if __name__ == "__main__":
    if len(argv) > 1:
        match argv[1]:
            case "random":
                print(get_random())
            case "all":
                todos = call_cmd(CMD)
                todos = list(filter(lambda elem: elem != b'', todos))
                for line in todos:
                    print(line)
                print(f"\nFound {len(todos)} things that need fixing!")
            case _:
                print("Usage: todo.py [random]")
    else:
        print(get_random())