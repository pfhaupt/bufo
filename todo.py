import subprocess
from typing import List
import random

from sys import argv

CMD = ["git", "grep", "-nE", "(todo!|TODO|FIXME|REVIEW)", "--", ":!todo.py"]

def call_cmd(cmd: List) -> int:
    return subprocess.check_output(cmd).split(b'\n')

def get_list() -> List[str]:
    todo = call_cmd(CMD)
    todo = list(filter(lambda elem: elem != b'', todo))
    todo = list(map(lambda elem: elem.decode("utf-8"), todo))
    todo = list(filter(lambda elem: elem != '' and not "raylib" in elem, todo))
    return todo

def get_random():
    todo = get_list()
    print(f"Found {len(todo)} things that need fixing!")
    if len(todo) == 0:
        return "DONE! No todo!(), TODO or FIXME found!"
    return random.choice(todo)

if __name__ == "__main__":
    if len(argv) > 1:
        match argv[1]:
            case "random":
                print(get_random())
            case "all":
                todo = get_list()
                for line in todo:
                    print(line)
                print(f"\nFound {len(todo)} things that need fixing!")
            case _:
                print("Usage: todo.py [random]")
    else:
        print(get_random())