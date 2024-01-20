MAX_MAX = 3
MAX_EXTERN = MAX_MAX # 10
MAX_CLASSES = MAX_MAX # 10
MAX_FIELDS = MAX_MAX # 10
MAX_FUNCTIONS = MAX_MAX # 10
MAX_STATEMENTS = MAX_MAX # 10
MAX_DEPTH = 5

from typing import List
import random
from typing import Callable
import os

class Fuzzer:
    limit: int
    threads: int
    classes: List[str]
    stmt_depth: int
    expr_depth: int
    call_cmd: Callable
    compiler_cmd: List[str]
    def __init__(self, call_cmd: Callable, compiler_cmd: List[str],limit: int = 100, threads: int = 1):
        self.limit = limit
        self.threads = threads
        self.classes = []
        self.stmt_depth = 0
        self.expr_depth = 0
        self.call_cmd = call_cmd
        self.compiler_cmd = compiler_cmd
        print(f"Fuzzer initialized with {limit} iterations and {threads} threads.")

    def generate_class(self) -> str:
        class_name = self.generate_identifier()
        class_name = class_name[0].upper() + class_name[1:]
        self.classes.append(class_name)
        src = f"class {class_name} {{\n"
        for _ in range(random.randint(0, MAX_FIELDS)):
            src += f"    {self.generate_field()}\n"
        for _ in range(random.randint(0, MAX_FUNCTIONS)):
            src += f"    {self.generate_func()}\n"
        for _ in range(random.randint(0, MAX_FUNCTIONS)):
            src += f"    {self.generate_constructor()}\n"
        src += "}\n"
        return src
    
    def generate_constructor(self) -> str:
        return f"constructor({self.generate_parameters()}) {{\n{self.generate_block()}}}\n"
    
    def generate_expression(self) -> str:
        if self.expr_depth >= MAX_DEPTH:
            return ""
        self.expr_depth += 1
        odds = random.randint(0, 100)
        src = ""
        if odds < 10:
            src = self.generate_identifier()
        elif odds < 20:
            src = self.generate_binary()
        elif odds < 30:
            src = self.generate_unary()
        elif odds < 40:
            src = self.generate_call()
        elif odds < 50:
            src = self.generate_field_access()
        else:
            src = self.generate_literal()
        self.expr_depth -= 1
        return src
        
    def generate_binary(self) -> str:
        return f"{self.generate_expression()} {random.choice(['+', '-', '*', '/', '%', '==', '!=', '<', '<=', '>', '>=', '&', '|', '^', '='])} {self.generate_expression()}"
    
    def generate_unary(self) -> str:
        return f"{random.choice(['-', '!'])}{self.generate_expression()}"
    
    def generate_arguments(self) -> str:
        return ", ".join([self.generate_expression() for _ in range(random.randint(0, MAX_STATEMENTS))])

    def generate_call(self) -> str:
        return f"{self.generate_identifier()}({self.generate_arguments()})"
    
    def generate_field_access(self) -> str:
        return f"{self.generate_expression()}.{self.generate_identifier()}"
    
    def generate_literal(self) -> str:
        odds = random.randint(0, 100)
        if odds < 20:
            return str(random.randint(-100, 100))
        elif odds < 30:
            return str(random.uniform(0, 100))
        elif odds < 40:
            return str(random.uniform(-100, 100))
        elif odds < 50:
            return random.choice(["true", "false"])
        else:
            return str(random.randint(0, 100))
    
    def generate_stmt_expression(self) -> str:
        return f"{self.generate_expression()};\n"
    
    def generate_stmt_let(self) -> str:
        return f"let {self.generate_identifier()} = {self.generate_expression()};\n"
    
    def generate_stmt_return(self) -> str:
        return f"return {self.generate_expression()};\n"
    
    def generate_stmt_if(self) -> str:
        odds = random.randint(0, 100)
        if odds < 50:
            return f"if {self.generate_expression()} {{\n{self.generate_block()}}}\n"
        else:
            return f"if {self.generate_expression()} {{\n{self.generate_block()}}} else {{\n{self.generate_block()}}}\n"
        
    def generate_stmt_while(self) -> str:
        return f"while {self.generate_expression()} {{\n{self.generate_block()}}}\n"
    
    def generate_stmt_break(self) -> str:
        return "break;\n"
    
    def generate_stmt_continue(self) -> str:
        return "continue;\n"
    
    def generate_statement(self) -> str:
        odds = random.randint(0, 100)
        if odds < 1:
            return self.generate_block()
        elif odds < 10:
            return self.generate_stmt_let()
        elif odds < 20:
            return self.generate_stmt_return()
        elif odds < 30:
            return self.generate_stmt_if()
        elif odds < 40:
            return self.generate_stmt_while()
        elif odds < 50:
            return self.generate_stmt_break()
        elif odds < 60:
            return self.generate_stmt_continue()
        else:
            return self.generate_stmt_expression()

    def generate_block(self) -> str:
        if self.stmt_depth >= MAX_DEPTH:
            return ""
        self.stmt_depth += 1
        src = "".join(["    " * self.stmt_depth + self.generate_statement() for _ in range(random.randint(0, MAX_STATEMENTS))])
        self.stmt_depth -= 1
        return src
    
    def generate_func(self) -> str:
        return f"func {self.generate_identifier()}({self.generate_parameters()}) -> {self.generate_type()} {{\n{self.generate_block()}}}\n"
    
    def generate_field(self) -> str:
        return f"{self.generate_identifier()}: {self.generate_type()};"
    
    def generate_type(self) -> str:
        if random.randint(0, 1) == 0 and len(self.classes) > 0:
            return random.choice(self.classes)
        else:
            return random.choice(["u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64", "f32", "f64", "bool"])

    def generate_parameter(self) -> str:
        return f"{self.generate_identifier()}: {self.generate_type()}"

    def generate_parameters(self) -> str:
        return ", ".join([self.generate_parameter() for _ in range(random.randint(0, MAX_STATEMENTS))])

    def generate_identifier(self) -> str:
        return "".join([chr(random.randint(ord('a'), ord('z'))) for _ in range(random.randint(1, 10))])

    def generate_extern(self) -> str:
        return f"extern {self.generate_identifier()}({self.generate_parameters()}) -> {self.generate_type()};\n"

    def generate_file(self) -> str:
        src = ""
        for _ in range(random.randint(0, MAX_EXTERN)):
            src += self.generate_extern()
        for _ in range(random.randint(0, MAX_CLASSES)):
            src += self.generate_class()
        for _ in range(random.randint(0, MAX_FUNCTIONS)):
            src += self.generate_func()
        src += "func main() -> i32 { return 0; }\n"
        return src

    def generate_bulk_code(self, thread: int, count: int):
        print(f"Thread {thread} started.")
        for i in range(count):
            code = self.generate_file()
            filename = f"./fuzz/invalid/fuzz_{thread}_{i}.bu"
            with open(filename, "w") as f:
                f.write(code)
            compiler_cmd = self.compiler_cmd + [filename]
            output = self.call_cmd(compiler_cmd)
            if output.returncode == 101:
                print(f"Thread {thread} generated invalid code.")
                with open(filename, "w") as f:
                    for line in output.stderr.decode("utf-8").split("\n"):
                        f.write(f"// {line}\n")
                    f.write("\n")
                    f.write(code)
            elif output.returncode == 0:
                os.rename(filename, f"./fuzz/valid/fuzz_{thread}_{i}.bu")
                print(f"Thread {thread} generated valid code.")
            else:
                os.remove(filename)
    
    def run(self):
        if not os.path.exists("./fuzz/invalid"):
            os.makedirs("./fuzz/invalid")
        if not os.path.exists("./fuzz/valid"):
            os.makedirs("./fuzz/valid")
        else:
            for file in os.listdir("./fuzz/valid"):
                os.remove(f"./fuzz/valid/{file}")
        from multiprocessing import Pool
        with Pool(self.threads) as p:
            p.starmap(self.generate_bulk_code, [(i, self.limit // self.threads) for i in range(self.threads)])
        pass