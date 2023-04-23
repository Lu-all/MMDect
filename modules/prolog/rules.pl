%%%%%%%%%%%%%
%Basic types%
%%%%%%%%%%%%%

command([add, cmp, sub, and, xor, test, lea, shl, shr, mov, push, pop, call, jmp, je, jne, jz, jg, jge, jl, jle, ret, syscall]).

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
    nonvar(X), number(X).
imm(X) :-
    var(X), random_between(0, 0xffff, X).

% MEM

mem(X) :-
    nonvar(X), string_chars(X, X1), length(X1, L), nth0(0, X1, '['), nth1(L, X1, ']').
mem(X) :-
    var(X),
    random_between(0x1000, 0xffff, B),
    number_chars(B, B1), append(['['],B1, X1), append(X1, [']'], X2),
    atom_chars(X, X2).

operation(Operand, O, Args):-
    Operand =.. [O|Args], nth0(_,[add,cmp,sub,and,xor,test,lea,shl,shr],O).

%%%%%%
%Type%
%%%%%%

type([],[]).

type([P], [R]):-
    arg_type(P,R).

type([P|Ps],[R|Rs]):-
    type(Ps, Rs),
    arg_type(P,R).

arg_type([],[]).

arg_type([P|Ps],[R|Rs]):-
    arg_type(Ps,Rs),
    assign(P,R),!.

arg_type([P],[R]):-
    assign(P,R).

assign(P,R):-
    reg(P), R=reg(P),!.

assign(P,R):-
    imm(P), R=imm(P),!.

assign(P,R):-
    mem(P),
    string_chars(P,C),
    delete(C, '[', C1),
    delete(C1, ']', C2),
    string_chars(S, C2),
    number_string(A, S),
    R=mem(A),!.

assign(P,R):-
    mem(P),
    string_chars(P,C),
    delete(C, '[', C1),
    delete(C1, ']', C2),
    atom_chars(A, C2),
    R=mem(A),!.

assign(P,P):-
    command(C),
    nth0(_, C, P), !.

assign(P,tag(P)).

%%%%%%%%%
%Compare%
%%%%%%%%%

compare_firm([],_,_,[]).

compare_firm([PLine|_Program], [Line], Name, Positive):-
    check(PLine, Line),
    Positive = [Name].

compare_firm([PLine|Program], [Line|Lines], Name, Positive):-
    check(PLine, Line),
    compare_firm(Program, Lines, Name, Positive).

apply_firm(Program, Signature, Name, Positive):-
    compare_firm(Program, Signature, Name, Positive).

apply_firm([_PLine|Program], Signature, Name, Positive):-
    apply_firm(Program, Signature, Name, Positive).

apply_firm(_, _, _, []).

check([],[]).

check(L, M):-
    L = M.

compare_firms([], _,_, []).
compare_firms(_,[],_,[]).

compare_firms(Program, [Firm|Firms], [Name|Names], Positives):-
    compare_firms(Program, Firms, Names, New_positives),
    apply_firm(Program, Firm, Name, Positive),
    append(New_positives, Positive, Positives),!.

%%%%%%%
%Parse%
%%%%%%%

% "i" <-> i
% Output number, not atom (in immediates)
string_atom(S,A):-
    var(A),
    number_string(A,S),
    !.

string_atom(S,A):-
    var(S),
    number(A), number_string(A,S), !.

% Output atom
string_atom(S,A):-
    atom_string(A,S).

% ["i", "a1", "a2"] <-> [i,a1,a2]
list_string_list_atom([],[]).

list_string_list_atom([S|Ss],[A|As]):-
    list_string_list_atom(Ss, As),
    string_atom(S,A).

% [["i", "a1", "a2"]["i","a"]] <->  [[i,a1,a2], [i,a]]
matrix_string_matrix_atom([],[]).

matrix_string_matrix_atom([S|Ss],[A|As]):-
    matrix_string_matrix_atom(Ss, As),
    list_string_list_atom(S, A).

% [i, a1, a2] <-> i(a1, a2)
atom_functor([tag(X)], tag(Y)):-
    atom_chars(X,C),
    delete(C, ':', C1),
    atom_chars(Y,C1),
    !.

atom_functor(X,Y):-
    Y =.. X.

% [[i, a1, a2], [i, a] <->  [i(a1,a2), i(a)]
matrix_atom_array_functor([], []).

matrix_atom_array_functor([X|Xs], [Y|Ys]) :-
	matrix_atom_array_functor(Xs, Ys),
	atom_functor(X,Y).

% [["i", "a1", "a2"]["i","a"]] -> [i(a1,a2), i(a)]
parse(Program, Parsed):-
    nonvar(Program),
    matrix_string_matrix_atom(Program, Atom_program),
	matrix_atom_array_functor(Atom_program,Parsed), !.

% [["i", "a1", "a2"]["i","a"]] <- [i(a1,a2), i(a)]
parse(Program, Parsed):-
    nonvar(Parsed),
    matrix_atom_array_functor(Atoms, Parsed),
    matrix_string_matrix_atom(Program, Atoms), !.

%%%%%%%
%Rules%
%%%%%%%

% i <-> o
% syntax
% rule(<name>, [<i>], [<o>]) :- types (reg, imm, mem)

% operations
% operation(op(R1, R2), op, [R1, R2])
% R1 xor R2 == xor(R1, R2)

% PUSH rules
rule(g1, [push(Imm), pop(Reg)], [mov(Reg, Imm)]) :- imm(Imm), reg(Reg).
rule(g2, [push(Reg), pop(Reg2)], [mov(Reg2, Reg)]) :- reg(Reg), reg(Reg2).
rule(g13, [push(Reg), 'ret'], [jmp(Reg)]) :- reg(Reg).

% MOV rules
rule(g3, [mov(Mem, Imm), push(Mem)], [push(Imm)]) :- mem(Mem), imm(Imm).
rule(g4, [mov(Mem, Reg), push(Mem)], [push(Reg)]) :- reg(Reg), mem(Mem).

rule(g7, [mov(Mem, Imm), Op1], [Op2]):-
    operation(Op1, O, [Reg, Mem]),
    operation(Op2, O, [Reg,Imm]),
    mem(Mem), imm(Imm), reg(Reg), nonvar(O).

rule(g8, [mov(Mem2, Mem), Op1], [Op2]) :-
    operation(Op1, O, [Reg, Mem2]),
    operation(Op2, O, [Reg,Mem]),
    mem(Mem), mem(Mem2), reg(Reg).

rule(g10, [mov(Mem, Reg), call(Mem)], [call(Reg)]) :- mem(Mem), reg(Reg).
rule(g11, [mov(Mem2, Mem), call(Mem2)], [call(Mem)]) :- mem(Mem), mem(Mem2).

rule(g12, [mov(Mem, Reg), jmp(Mem)], [jmp(Reg)]) :- mem(Mem), reg(Reg).
rule(g14, [mov(Mem2, Mem), jmp(Mem2)], [jmp(Mem)]) :- mem(Mem), mem(Mem2).

% POP rules
rule(g5, [pop(Mem2), mov(Mem, Mem2)], [pop(Mem)]) :- mem(Mem), mem(Mem2).
rule(g6, [pop(Mem), mov(Reg, Mem)], [pop(Reg)]) :- mem(Mem), reg(Reg).

rule(g9, [pop(Mem), push(Mem)], []) :- mem(Mem).
rule(f19, [pop(Reg), push(Reg)], []) :- reg(Reg).
rule(g15, [pop(Mem), jmp(Mem)], [ret]) :- mem(Mem).

% CONDITIONAL BRANCH rules
rule(f16, [je(J), jne(J)], [jmp(J)]).
rule(f16_2, [jne(J), je(J)], [jmp(J)]).
rule(f17, [jl(J), jge(J)], [jmp(J)]).
rule(f17_2, [jge(J), jl(J)], [jmp(J)]).
rule(f18, [jg(J), jle(J)], [jmp(J)]).
rule(f18_2, [jle(J), jg(J)], [jmp(J)]).

%%%%%%
%Main%
%%%%%%

generate_and_compare([],_,_,_,[]).
generate_and_compare(_,_,[],_,[]).
generate_and_compare(_,_,_,[],[]).
generate_and_compare(Program, Generated, Firms, Names, Positives):-
	parse(Program, Parsed), % Parse to Functors
	rules([], Parsed, Generated),% Apply rules
    matrix_atom_array_functor(Array_program,Generated),
    type(Array_program, Typed_program),
    matrix_atom_array_functor(Typed_program, Result),
    compare_firms(Result, Firms, Names, Positives).

generate([],[]).
generate(Program, Result) :-
	parse(Program, Parsed), % Parse to Functors
	rules([], Parsed, New_program), % Apply rules
    parse(Result, New_program).

compare([],_,_,[]).
compare(_,[],_,[]).
compare(_,_,[],[]).
compare(Program, Firms, Names, Positives):-
    matrix_string_matrix_atom(Program, Atoms),
    type(Atoms, Typed_atoms),
    matrix_atom_array_functor(Typed_atoms, Result),
    compare_firms(Result, Firms, Names, Positives).

rules(Result,[], Result).

rules(Program_before,[Last], Result):-
    append(Program_before, [Last], Result).

rules(Program_before, [N1,N2|Program_next], Result) :-
	rule(_, [N1,N2], R),
	append(Program_before, R, New_program_before),
    rules(New_program_before, Program_next, Result).

rules(Program_before, [N1,N2|Program_next], Result) :-
    append(Program_before, [N1], New_program_before),
    rules(New_program_before, [N2|Program_next], Result).
