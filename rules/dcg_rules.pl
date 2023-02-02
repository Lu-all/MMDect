%%%%%%%%%%
%Examples%
%%%%%%%%%%
etc_shadow_sign([mov(reg(Reg),imm(0x6477737361702FFF)),
op(shr,reg(Reg),imm(8)),
push(reg(Reg)),
mov(reg(Reg),imm(0xFFFFFFFF6374652F)),
op(shl,reg(Reg),imm(32)),
push(reg(Reg))]).

test_sign([
mov(mem(123), imm(_Imm)),
push(mem(123)),
push(imm(_Imm2)),
mov(reg(r13), imm(_Imm3))]).

test([
    ["mov", "[123]","0x6477737361702FFF"],
    ["push", "[123]"],
    ["push", "12"],
    ["pop", "r12"],
    ["push", "r12"],
    ["mov", "r13", "13"],
    ["mov", "r12", "0xFFFFFFFF6374652F"],
    ["xor", "rax", "rax"]
    ]).
    
    test_special([
    ["mov", "[123]","0x6477737361702FFF"],
    ["push", "[123]"],
    ["push", "12"],
    ["pop", "r12"],
    ["mov", "r13", "13"],
    ["mov", "r12", "0xFFFFFFFF6374652F"],
    ["xor", "rax", "rax"],
    ["syscall"],
    ["jne", "loop_read"],
    ["close_file:"]
    ]).
    
    test_long([
    ["mov", "r12", "0x6477737361702FFF"],
    ["shr", "r12", "8"],
    ["mov", "[r13]", "r12"],
    ["push", "[r13]"],
    ["mov", "r12", "0xFFFFFFFF6374652F"],
    ["shl", "r12", "32"],
    ["push", "r12"],
    ["mov", "rdi", "rsp"],
    ["add", "rdi", "4"],
    ["xor", "rsi", "rsi"],
    ["xor", "rdx", "rdx"],
    ["pop", "r15"],
    ["push", "r15"],
    ["push", "0x2"],
    ["pop", "rax"],
    ["syscall"],
    ["push", "rax"],
    ["pop", "r12"],
    ["mov", "r13", "0x100"],
    ["sub", "rsp", "r13"],
    ["loop_read:"],
    ["mov", "rdi", "r12"],
    ["mov", "rsi", "rsp"],
    ["mov", "rdx", "r13"],
    ["xor", "rax", "rax"],
    ["syscall"],
    ["xor", "r14", "r14"],
    ["cmp", "rax", "r14"],
    ["je", "close_file"],
    ["mov", "rdx", "rax"],
    ["mov", "r14", "0x1"],
    ["mov", "rdi", "0x1"],
    ["mov", "rsi", "rsp"],
    ["mov", "rax", "0x1"],
    ["syscall"],
    ["cmp", "rax", "r14"],
    ["je", "loop_read"],
    ["jne", "loop_read"],
    ["close_file:"],
    ["mov", "rdi", "r12"],
    ["mov", "r14", "0x1"],
    ["mov", "rax", "r14"],
    ["syscall"],
    ["xor", "rdi", "rdi"],
    ["mov", "r14", "0x3c"],
    ["mov", "rax", "r14"],
    ["syscall"]
    ]).
    
%%%%%%%%%%%%%
%Basic types%
%%%%%%%%%%%%%

% REG
% reg(Reg) <--> ["Reg"]
register(reg(ax)) --> ["ax"].
register(reg(bx)) --> ["bx"].
register(reg(cx)) --> ["cx"].
register(reg(dx)) --> ["dx"].
register(reg(ah)) --> ["ah"].
register(reg(bh)) --> ["bh"].
register(reg(ch)) --> ["ch"].
register(reg(dh)) --> ["dh"].
register(reg(al)) --> ["al"].
register(reg(bl)) --> ["bl"].
register(reg(cl)) --> ["cl"].
register(reg(dl)) --> ["dl"].
register(reg(si)) --> ["si"].
register(reg(di)) --> ["di"].
register(reg(bp)) --> ["bp"].
register(reg(sp)) --> ["sp"].
register(reg(es)) --> ["es"].
register(reg(cs)) --> ["cs"].
register(reg(ds)) --> ["ds"].
register(reg(ss)) --> ["ss"].
register(reg(gs)) --> ["gs"].
register(reg(fs)) --> ["fs"].
register(reg(eax)) --> ["eax"].
register(reg(ebx)) --> ["ebx"].
register(reg(ecx)) --> ["ecx"].
register(reg(edx)) --> ["edx"].
register(reg(esp)) --> ["esp"].
register(reg(ebp)) --> ["ebp"].
register(reg(eip)) --> ["eip"].
register(reg(esi)) --> ["esi"].
register(reg(edi)) --> ["edi"].
register(reg(rax)) --> ["rax"].
register(reg(rbx)) --> ["rbx"].
register(reg(rip)) --> ["rip"].
register(reg(rcx)) --> ["rcx"].
register(reg(rdx)) --> ["rdx"].
register(reg(rsp)) --> ["rsp"].
register(reg(rbp)) --> ["rbp"].
register(reg(rsi)) --> ["rsi"].
register(reg(rdi)) --> ["rdi"].
register(reg(r8)) --> ["r8"].
register(reg(r9)) --> ["r9"].
register(reg(r10)) --> ["r10"].
register(reg(r11)) --> ["r11"].
register(reg(r12)) --> ["r12"].
register(reg(r13)) --> ["r13"].
register(reg(r14)) --> ["r14"].
register(reg(r15)) --> ["r15"].

% IMM
% imm(Imm) <--> ["Imm"]
immediate(imm(I)) --> [A], {number_string(I,A)}.

% MEM

% mem(Mem) --> ["[Mem]"]
memory(mem(A)) --> 
    [S],{
          var(S),
          atom_string(A,S1),
          string_concat(S1,"]",S2),
          string_concat("[",S2,S)
        }.

% mem(Mem) <-- ["[Mem]"]
memory(mem(A)) --> 
    [S],{
        	nonvar(S),
          	string_concat("[",S1,S),
            string_concat(M,"]",S1),
          	atom_string(A,M)
        }.

% args

argument(A) --> register(A).
argument(A) --> memory(A).
argument(A) --> immediate(A).
arguments([A]) --> argument(A).
arguments([A|As]) --> argument(A), arguments(As).

% instruction
op_type(A) --> i_add(A).
op_type(A) --> i_cmp(A).
op_type(A) --> i_sub(A).
op_type(A) --> i_and(A).
op_type(A) --> i_xor(A).
op_type(A) --> i_test(A).
op_type(A) --> i_lea(A).
op_type(A) --> i_shl(A).
op_type(A) --> i_shr(A).

operation(add) --> ["add"].
operation(cmp) --> ["cmp"].
operation(sub) --> ["sub"].
operation(and) --> ["and"].
operation(xor) --> ["xor"].
operation(test) --> ["test"].
operation(lea) --> ["lea"].
operation(shl) --> ["shl"].
operation(shr) --> ["shr"].
command(C) --> operation(C).
command(mov) --> ["mov"].
command(push) --> ["push"].
command(pop) --> ["pop"].
command(call) --> ["call"].

command(jmp) --> ["jmp"].
command(je) --> ["je"].
command(jne) --> ["jne"].
command(jz) --> ["jz"].
command(jz) --> ["jz"].
command(jg) --> ["jg"].
command(jge) --> ["jge"].
command(jl) --> ["jl"].
command(jle) --> ["jle"].

command(ret) --> ["ret"].
command(syscall) --> ["syscall"].

i_add(add(A1,A2)) --> ["add"], argument(A1), argument(A2).
i_cmp(cmp(A1,A2)) --> ["cmp"], argument(A1), argument(A2).
i_sub(sub(A1,A2)) --> ["sub"], argument(A1), argument(A2).
i_and(and(A1,A2)) --> ["and"], argument(A1), argument(A2).
i_xor(xor(A1,A2)) --> ["xor"], argument(A1), argument(A2).
i_test(test(A1,A2)) --> ["test"], argument(A1), argument(A2).
i_lea(lea(A1,A2)) --> ["lea"], argument(A1), argument(A2).
i_shl(shl(A1,A2)) --> ["shl"], argument(A1), argument(A2).
i_shr(shr(A1,A2)) --> ["shr"], argument(A1), argument(A2).

i_mov(mov(A1,A2)) --> ["mov"], argument(A1), argument(A2).
i_push(push(A1)) --> ["push"], argument(A1).
i_pop(pop(A1)) --> ["pop"], argument(A1).
i_call(call(A1)) --> ["call"], argument(A1).

i_jmp(jmp(A1)) --> ["jmp"], argument(A1).
i_je(je(A1)) --> ["je"], argument(A1).
i_jne(jne(A1)) --> ["jne"], argument(A1).
i_jz(jz(A1)) --> ["jz"], argument(A1).
i_jnz(jz(A1)) --> ["jnz"], argument(A1).
i_jg(jg(A1)) --> ["jg"], argument(A1).
i_jge(jge(A1)) --> ["jng"], argument(A1).
i_jl(jl(A1)) --> ["jl"], argument(A1).
i_jle(jle(A1)) --> ["jnl"], argument(A1).

i_ret(ret) --> ["ret"].
i_syscall(syscall) --> ["syscall"].

%instruction(A) --> op_type(A).
%instruction(A) --> i_mov(A).
%instruction(A) --> i_push(A).
%instruction(A) --> i_pop(A).
%instruction(A) --> i_call(A).
%instruction(A) --> i_jmp(A).
%instruction(A) --> i_jz(A).
%instruction(A) --> i_jnz(A).
%instruction(A) --> i_je(A).
%instruction(A) --> i_jne(A).
%instruction(A) --> i_jg(A).
%instruction(A) --> i_jge(A).
%instruction(A) --> i_jl(A).
%instruction(A) --> i_ret(A).
%instruction(A) --> i_syscall(A).

instruction([C,As]) --> command(C), arguments(As).
% program

program([I]) --> instruction(I).
program([I|P]) --> instruction(I), program(P).

%%%%%%%%%%%
%Functions%
%%%%%%%%%%%

parser(List, Program) :-
    flatten(List, Flat),
    phrase(program(Program),Flat).

parser(List, Program) :-
    phrase(program(Program),List).

/** <examples>
?- parser(L, [mov(reg(rax),imm(0x12)), mov(reg(rax),mem(ax))]).
?- test(L), parser(L, P).
?- phrase(memory(B),["[rax]"]).
?- phrase(memory(mem('[',rax,']')),A).
*/