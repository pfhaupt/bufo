let a: i32 = 12;
let b: i32 = 13;
let c: i32 = a + b;
do_stuff(c);

%a = alloca i32     // ???(StackPointer(0))
%b = alloca i32     // ???(StackPointer(4))
%c = alloca i32     // ???(StackPointer(8))
store %a, i32 12    // Store(StackPointer(0), Immediate(12))
store %b, i32 13    // Store(StackPointer(4), Immediate(13))
%0 = load i32 %a    // ???(StackPointer(0))
%1 = load i32 %b    // ???(StackPointer(4))
%2 = add i32 %0, %1 // Add(???, StackValue(0), StackValue(4))
store %c, i32 %2    // Add(StackPointer(8), ...)
%3 = load i32, %c   // ???(StackPointer(8))
call do_stuff %3    // Call(do_stuff, StackValue(8))

Store(StackPointer(0), Immediate(12))
Store(StackPointer(4), Immediate(13))
Add(StackPointer(8), StackValue(0), StackValue(4))
Call(do_stuff, StackValue(8))

mov [rsp - 0], 12
mov [rsp - 4], 13
mov rax, [rsp - 0]
add rax, [rsp - 4]
mov [rsp - 8], rax
mov rcx, [rsp - 8]
call do_stuff

// mem2reg
%a = i32 12
%b = i32 13
%c = add i32 %a, %b
call do_stuff %c

Add(Reg(0), Imm(12), Imm(13))
Call(do_stuff, Reg(0))

mov rax, 12
add rax, 13
mov rcx, rax // a smart register allocator would not do that :kapp:
call do_stuff

// constant folding
call do_stuff 25

Call(do_stuff, Imm(25))

mov rcx, 25
call do_stuff

