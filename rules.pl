%%%%%%%%%%
%Examples%
%%%%%%%%%%
program([['mov', 'r12', '0x6477737361702FFF'],
 ['shr', 'r12', '8'],
 ['mov', '[r13]', 'r12'],
 ['push', '[r13]'],
 ['mov', 'r12', '0xFFFFFFFF6374652F'],
 ['shl', 'r12', '32'],
 ['push', 'r12'],
 ['mov', 'rdi', 'rsp'],
 ['add', 'rdi', '4'],
 ['xor', 'rsi', 'rsi'],
 ['xor', 'rdx', 'rdx'],
 ['pop', 'r15'],
 ['push', 'r15'],
 ['push', '0x2'],
 ['pop', 'rax'],
 ['syscall'],
 ['push', 'rax'],
 ['pop', 'r12'],
 ['mov', 'r13', '0x100'],
 ['sub', 'rsp', 'r13'],
 ['loop_read:'],
 ['mov', 'rdi', 'r12'],
 ['mov', 'rsi', 'rsp'],
 ['mov', 'rdx', 'r13'],
 ['xor', 'rax', 'rax'],
 ['syscall'],
 ['xor', 'r14', 'r14'],
 ['cmp', 'rax', 'r14'],
 ['je', 'close_file'],
 ['mov', 'rdx', 'rax'],
 ['mov', 'r14', '0x1'],
 ['mov', 'rdi', '0x1'],
 ['mov', 'rsi', 'rsp'],
 ['mov', 'rax', '0x1'],
 ['syscall'],
 ['cmp', 'rax', 'r14'],
 ['je', 'loop_read'],
 ['jne', 'loop_read'],
 ['close_file:'],
 ['mov', 'rdi', 'r12'],
 ['mov', 'r14', '0x1'],
 ['mov', 'rax', 'r14'],
 ['syscall'],
 ['xor', 'rdi', 'rdi'],
 ['mov', 'r14', '0x3c'],
 ['mov', 'rax', 'r14'],
 ['syscall']]).

test([
[mov, '[123]','0x6477737361702FFF'],
[push, '[123]'],
[push, '12'],
[pop, 'r12'],
[mov, r13, '13'],
[mov, r12, '0xFFFFFFFF6374652F']
]).

%%%%%%%%%%%%%
%Basic types%
%%%%%%%%%%%%%

% REG

reg(eax).
reg(ebx).
reg(ecx).
reg(edx).
reg(esp).
reg(ebp).
reg(esi).
reg(edi).
reg(rax).
reg(rbx).
reg(rip).
reg(rcx).
reg(rdx).
reg(rsp).
reg(rbp).
reg(rsi).
reg(rdi).
reg(r8).
reg(r9).
reg(r10).
reg(r11).
reg(r12).
reg(r13).
reg(r14).
reg(r15).

% IMM

imm(X) :-
    nonvar(X), atom_number(X, XN), number(XN).
imm(X) :-
    var(X), random_between(0, 0xffff, XN), atom_number(X, XN).

% MEM

mem(X) :-
    nonvar(X), string_chars(X, X1), length(X1, L), nth0(0, X1, '['), nth1(L, X1, ']').
mem(X) :-
    var(X), random_between(0x1000, 0xffff, B), number_chars(B, B1), append(['['],B1, X1), append(X1, [']'], X2), atom_chars(X, X2).

%%%%%%%%%%%
%Functions%
%%%%%%%%%%%

% Matrix of str <-> Array of Functors
parser(X, Y):-
    var(X),var(Y),!.

parser([], []).

parser([X|Xs], [Y|Ys]) :-
	parser(Xs, Ys),
	Y =.. X.


% Array is input
% Once Tail is deduced, Head & Body are resolved
append(_Head,[],[],[]).
append(Head,Body,Tail, Array) :-
    append(Extra, Tail, Array),
    append(Head,Body,Extra).

% Array is output
% Once Head & Body are merged, Tail is merged too.
substitute(_Head,[],[],[]).
substitute(Head,Body,Tail, Array) :-
    append(Head,Body,Extra),
    append(Extra, Tail, Array).

% Try to apply any rule possible
apply_rules(Head, Body, Tail,Program) :-
    rule(_X,Body, Result),
    substitute(Head,Result,Tail,Program).

% No possibilities left
rules([], _Program).

% Try to apply rule and go on with rest of possibilities
rules([Cut|_Cuts], Program) :-
    nth1(1,Cut,Head),
    nth1(2,Cut,Body),
    nth1(3,Cut,Tail),
    apply_rules(Head, Body, Tail, Program).

% No rules applicable, go on with rest of possibilities
rules([_Cut|Cuts], Program) :-
    rules(Cuts, Program).

%%%%%%%
%Rules%
%%%%%%%

rule(1, [push(Imm), pop(Reg)], [mov(Reg, Imm)]) :- imm(Imm), reg(Reg).
rule(2, [push(Reg), pop(Reg2)], [mov(Reg2, Reg)]) :- reg(Reg), reg(Reg1).
rule(3, [mov(Mem, Imm), push(Mem)], [push(Imm)]) :- mem(Mem), imm(Imm).

%%%%%%
%Main%
%%%%%%

main(P,R) :-
	parser(P,Parsed), % Parse to Functors
    setof([Head,Body,Tail],(append(Head, Body, Tail, Parsed)),Cuts), % Get all possibilities of append
 	rules(Cuts,Applied), % Apply rules
    ((   length(Applied,L), L < 1, R=P, !); % Rule applied, parse to matrix
    parser(R,Applied)). % No rule applied
