type("eax", reg).
type("ebx", reg).
type("ecx", reg).
type("edx", reg).
type("esp", reg).
type("ebp", reg).
type("esi", reg).
type("edi", reg).
type("rax", reg).
type("rbx", reg).
type("rip", reg).
type("rcx", reg).
type("rdx", reg).
type("rsp", reg).
type("rbp", reg).
type("rsi", reg).
type("rdi", reg).
type("r8", reg).
type("r9", reg).
type("r10", reg).
type("r11", reg).
type("r12", reg).
type("r13", reg).
type("r14", reg).
type("r15", reg).
type("pop", operation).
type("push", operation).
type("nop", operation).
type("mov", operation).
type("cmp", operation).
type("add", operation).
type("sub", operation).
type("and", operation).
type("xor", operation).
type("test", operation).
type("lea", operation).
type("shr", operation).
type("shl", operation).
type("jz", conditional_branch).
type("je", conditional_branch).
type("jne", conditional_branch).
type("jl", conditional_branch).
type("jle", conditional_branch).
type("jg", conditional_branch).
type("jge", conditional_branch).
type("jmp", branch).
type("call", call).
type("syscall", call).

reg(X) :- type(X, reg).
imm(X) :- number(X); var(X), random_between(0, 0xffff, X).
operation(X) :- type(X, operation).
conditional_branch(X) :- type(X, conditional_branch).
branch(X) :- type(X, branch).
call_type(X) :- type(X, call).

mem(X) :- nonvar(X), imm(X), !, fail.
mem(X) :- nonvar(X), reg(X), !, fail.
mem(X) :- nonvar(X), string_chars(X, X1), length(X1, L), nth0(0, X1, '['), nth1(L, X1, ']') ; var(X), random_between(0x1000, 0xffff, B), number_chars(B, B1), append(['['],B1, X1), append(X1, [']'], X2), string_chars(X, X2).

% reglas

%%%%%%%%%%%%%%%%
%% PUSH rules %%
%%%%%%%%%%%%%%%%
% regla 1
% PUSH Imm / POP Reg  <-->  MOV Reg,Imm
% Use: r1(["push", 4], ["pop", "rax"], ["mov", "rax", 4]).
r1([Push, Imm], [Pop, Reg], [Mov, Reg, Imm]) :-
    Push = "push", Pop = "pop", Mov = "mov",
    imm(Imm), reg(Reg).
                                             
% regla 2
% PUSH Reg / POP Reg2  <-->  MOV Reg2,Reg
% Use: r2(["push", "rbx"], ["pop", "rax"], ["mov", "rax", "rbx"]).
r2([Push, Reg], [Pop, Reg2], [Mov, Reg2, Reg]) :-
    Push = "push", Pop = "pop", Mov = "mov",
    reg(Reg), reg(Reg2).         

% regla 13
% PUSH Reg / RET  <-->  jmp Reg
% Use: r13(["push", "rax"], ["ret"], ["jmp", "rax"]).
r13([Push, Reg], [Ret], [Jmp, Reg]) :-
    Push = "push", Ret = "ret",Jmp = "jmp",
    reg(Reg).      

%%%%%%%%%%%%%%%
%% MOV rules %%
%%%%%%%%%%%%%%%
% regla 3
% MOV Mem,Imm / PUSH Mem	<-->  PUSH Imm
% Use: r3(["mov", "[1]", 4], ["push", "[1]"], ["push", 4]).
r3([Mov, Mem, Imm], [Push, Mem], [Push, Imm]) :-
    Mov = "mov", Push = "push", Push = "push",
    mem(Mem), imm(Imm).

% regla 4
% MOV Mem,Reg / PUSH Mem   <-->  PUSH Reg
% Use: r4(["mov", "[1]", "rax"], ["push", "[1]"], ["push", "rax"]).
r3([Mov, Mem, Reg], [Push, Mem], [Push, Reg]) :-
    Mov = "mov", Push = "push",
    mem(Mem), reg(Reg).

% regla 7
% MOV Mem,Imm / OP Reg,Mem  <-->  OP Reg,Imm
% Use: r7(["mov", "[1]", 4], ["add", "rax", "[1]"], ["add", "rax", 4]).
r7([Mov, Mem, Imm], [OP, Reg, Mem], [OP, Reg, Imm]) :-
    Mov = "mov", operation(OP),
    mem(Mem), reg(Reg), imm(Imm).

% regla 8
% MOV Mem2,Mem / OP Reg,Mem2  <-->  OP Reg,Mem
% Use: r8(["mov", "[2]", "[1]"], ["add", "rax", "[2]"], ["add", "rax", "[1]"]).
r8([Mov, Mem2, Mem], [OP, Reg, Mem2], [OP, Reg, Mem]) :-
    Mov = "mov", operation(OP),
    mem(Mem), reg(Reg), mem(Mem2).

% regla 10
% MOV Mem,Reg / CALL Mem  <-->  CALL Reg
% Use: r10(["mov", "[2]", "rax"], ["call", "[2]"], ["call", "rax"]).
r10([Mov, Mem, Reg], [Call, Mem], [Call, Reg]) :-
    Mov = "mov", call_type(Call),
    mem(Mem), reg(Reg).

% regla 11
% MOV Mem2,Mem / CALL Mem2  <-->  CALL Mem
% Use: r11(["mov", "[2]", "[1]"], ["call", "[2]"], ["call", "[1]"]).
r11([Mov, Mem2, Mem], [Call, Mem2], [Call, Mem]) :-
    Mov = "mov", call_type(Call),
    mem(Mem), mem(Mem2).

% regla 12
% MOV Mem,Reg / JMP Mem  <-->  JMP Reg
% Use: r12(["mov", "[2]", "rax"], ["jmp", "[2]"], ["jmp", "rax"]).
r12([Mov, Mem, Reg], [Jmp, Mem], [Jmp, Reg]) :-
    Mov = "mov", branch(Jmp),
    mem(Mem), reg(Reg).

% regla 14
% MOV Mem2,Mem / JMP Mem2  <-->  JMP Mem
% Use: r14(["mov", "[2]", "[1]"], ["jmp", "[2]"], ["jmp", "[1]"]).
r14([Mov, Mem2, Mem], [Jmp, Mem2], [Jmp, Mem]) :-
    Mov = "mov", branch(Jmp),
    mem(Mem), mem(Mem2).

%%%%%%%%%%%%%%%
%% POP rules %%
%%%%%%%%%%%%%%%
% regla 5
% POP Mem2 / MOV Mem,Mem2	<-->  POP Mem
% Use: r5(["pop", "[2]"], ["mov", "[1]", "[2]"], ["pop", "[1]"]).
r5([Pop, Mem2], [Mov, Mem, Mem2], [Pop, Mem]) :-
    Mov = "mov", Pop = "pop",
    mem(Mem), mem(Mem2).

% regla 6
% POP Mem / MOV Reg,Mem  <-->  POP Reg
% Use: r6(["pop", "[2]"], ["mov", "rax", "[2]"], ["pop", "rax"]).
r6([Pop, Mem], [Mov, Reg, Mem], [Pop, Reg]) :-
    Mov = "mov", Pop = "pop",
    mem(Mem), reg(Reg).

% regla 9
% POP Mem / PUSH Mem  <-->  NOP
% Use: r9(["pop", "[2]"], ["push", "[2]"], ["nop"]).
r9([Pop, Mem], [Push, Mem], [Nop]) :-
    Push = "push", Pop = "pop", Nop = "nop",
    mem(Mem).

% regla 20
% POP Reg / PUSH Reg <--> NOP
% Use: r20(["pop", "rax"], ["push", "rax"], ["nop"]).
r20([Pop, Reg], [Push, Reg], [Nop]) :-
    Push = "push", Pop = "pop", Nop = "nop",
    reg(Reg).

% regla 15
% POP Mem / JMP Mem  <-->  RET
% Use: r15(["pop", "[2]"], ["jmp", "[2]"], ["ret"]).
r15([Pop, Mem], [Jmp, Mem], [Ret]) :-
    Jmp = "jmp", Pop = "pop", Ret ="ret",
    mem(Mem).

%%%%%%%%%%%%%%%%%%%%%%%
%% COND_BRANCH rules %%
%%%%%%%%%%%%%%%%%%%%%%%
% regla 17
% JE Reg / JNE Reg <---> JMP Reg
% Use: r17(["je", "rax"], ["jne", "rax"], ["jmp", "rax"]).
r17([Je, Reg], [Jne, Reg], [Jmp, Reg]) :-
    Jmp = "jmp", reg(Reg),
    ((   Jne = "je", Je = "jne");
    (    Jne = "jne",Je = "je" )).

% regla 18
% JL Reg / JGE Reg <---> JMP Reg
% Use: r18(["jl", "rax"], ["jge", "rax"], ["jmp", "rax"]).
r18([Jl, Reg], [Jge, Reg], [Jmp, Reg]) :-
    Jmp = "jmp", reg(Reg),
    ((   Jge = "jl", Jl = "jge");
    (    Jge = "jge",Jl = "jl" )).

% regla 19
% JG Reg / JLE Reg <---> JMP Reg
% Use: r19(["jg", "rax"], ["jle", "rax"], ["jmp", "rax"]).
r19([Jg, Reg], [Jle, Reg], [Jmp, Reg]) :-
    Jmp = "jmp", reg(Reg),
    ((   Jle = "jg", Jg = "jle");
    (    Jle = "jle",Jg = "jg" )).

%%%%%%%%%%%%%%%%%%%%%%
%% ARITHMETIC rules %%
%%%%%%%%%%%%%%%%%%%%%%
% regla 16
% XOR Reg, Reg <---> MOV Reg1, 0
% Use: r16(["xor", "rax", "rax"], ["mov", "rax", 0]).
r16([Xor, Reg, Reg], [Mov, Reg, Imm]) :-
    Xor = "xor", Mov = "mov",
    reg(Reg), Imm = 0.
