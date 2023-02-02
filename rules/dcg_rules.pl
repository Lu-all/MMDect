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
immediate(imm(I)) --> [A], {(nonvar(I);nonvar(A)),number_string(I,A)}.

% MEM

% mem(Mem) --> ["[Mem]"]
memory(mem(A)) --> 
    [S],{
          nonvar(A),
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

% TAG
% tag(Tag) --> ["Tag"]
tags(tag(A)) --> 
    [S],{
        	atom_string(A,S),
          	\+phrase(memory(A),S),
          	\+phrase(immediate(A),S),
          	\+phrase(register(A),S)
        }.

% args

argument(A) --> register(A),!.
argument(A) --> memory(A),!.
argument(A) --> immediate(A),!.
argument(A) --> tags(A).
arguments([A]) --> argument(A).
arguments([A|As]) --> argument(A), arguments(As).

% instruction
operation(add) --> ["add"].
operation(cmp) --> ["cmp"].
operation(sub) --> ["sub"].
operation(and) --> ["and"].
operation(xorr) --> ["xor"].
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

command(tag(A)) -->
    [T], {
          nonvar(A),
          atom_string(A,T1),
          string_concat(T1,":",T)
        }.

command(tag(A)) --> 
    [T],{
        	nonvar(T),
            string_concat(T1,":",T),
          	atom_string(A,T1)
        }.
    
         
instruction([C,As]) --> command(C), arguments(As).
% program

program([I]) --> instruction(I).
program([I,P]) --> instruction(I), program(P).

%%%%%%%%%%%
%Functions%
%%%%%%%%%%%    
    
% Parse flow:
% [["i", "a1", "a2"]["i","a"]] <--> [i[a1,a2], i[a]] <--> [i(a1,a2), i(a)]

%[["i", "a1", "a2"]["i","a"]] --> [i(a1,a2), i(a)]
parser(List, Program) :-
    nonvar(List),
    to_atoms(List, Flat),
    to_functors(Flat,Program).

%[["i", "a1", "a2"]["i","a"]] <-- [i(a1,a2), i(a)]
parser(List, Program) :-
    nonvar(Program),
    to_functors(Flat, Program),
    to_atoms(List, Flat).

% [["i", "a1", "a2"]["i","a"]] <--> [[i,[a1,a2]], [i,[a]]]
to_atoms([], []).

to_atoms([Co|[]], [Cr|[]]) :-
    (atom(Cr);string(Co);functor(Cr,tag, 1)),
    phrase(command(Cr), [Co]).

to_atoms([Co|O], [Cr|R]) :-
    (atom(Cr);string(Co)),
    (nonvar(O);nonvar(R)),
    phrase(command(Cr), [Co]),
    R = [R1],
    phrase(arguments(R1), O).

to_atoms([O|Os], [R|Rs]) :-
    to_atoms(O,R),
    to_atoms(Os,Rs).

to_functors([], []).
% [[i, [a1, a2]], [i, [a]] --> [i(a1,a2), i(a)]
to_functors([X|Xs], [Y|Ys]) :-
    nonvar(X),
    my_flatten(X, Xf),
	to_functor(Xf, Y),
    to_functors(Xs, Ys).

% [[i, [a1, a2]], [i, [a]] <-- [i(a1,a2), i(a)]
to_functors([X|Xs], [Y|Ys]) :-
    nonvar(Y),
	to_functor(Xf, Y),
    my_flatten(X, Xf),
    to_functors(Xs, Ys).

% [i, a1, a2] <--> i(a1,a2)
to_functor([],[]).
to_functor([tag(T)], tag(T)):-!.
to_functor(X, Y) :-
    Y =.. X.

% [i, a1, a2] <--> [i, [a1, a2]]
my_flatten([tag(T)], [tag(T)]).
my_flatten([X,Args], [X|Args]):-
    atom(X);string(X).
my_flatten([X], [X]):-
    atom(X);string(X).

my_flatten([X|Xs], [Y|Ys]):-
    my_flatten(X,Y),
    my_flatten(Xs,Ys).


/** <examples>
?- parser(L, [mov(reg(rax),imm(0x12)), mov(reg(rax),mem(ax))]).
?- test(L), parser(L, P).
?- parser([["mov", "[123]","0x6477737361702FFF"],["xor", "rax", "rax"]], P).
?- myflatten([["mov", "[123]","0x6477737361702FFF"],["xor", "rax", "rax"]], R).
*/