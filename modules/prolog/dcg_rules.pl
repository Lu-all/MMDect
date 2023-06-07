%%%%%%%%%%%%%
%Basic types%
%%%%%%%%%%%%%
instruction([C]) -->
    command(C).

instruction([C|[A]]) -->
    command(C), arguments(A).

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
        number(A),
        number_string(A,S1),
        string_concat(S1,"]",S2),
        string_concat("[",S2,S)
        }.

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
            number_string(A,M)
        }.

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
            atom_string(A,S)
        }.

% args

argument(A) --> register(A),!.
argument(A) --> memory(A),!.
argument(A) --> immediate(A),!.
argument(A) --> tags(A).
arguments([A]) --> argument(A).
arguments([A|As]) --> argument(A), arguments(As).

% instruction
operation(Operand, O, Args):-
    Operand =.. [O|Args], nth0(_,[add,cmp,sub,and,xor,test,lea,shl,shr],O).

command(add) --> ["add"].
command(cmp) --> ["cmp"].
command(sub) --> ["sub"].
command(and) --> ["and"].
command(xor) --> ["xor"].
command(test) --> ["test"].
command(lea) --> ["lea"].
command(shl) --> ["shl"].
command(shr) --> ["shr"].
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
%        nonvar(A),
        atom(A),
        atom_string(A,T1),
        string_concat(T1,":",T)
        }.

command(tag(A)) -->
    [T],{
%            nonvar(T),
            string(T),
            string_concat(T1,":",T),
            atom_string(A,T1)
        }.

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

% Parse flow:
% [["i", "a1", "a2"]["i","a"]] <--> [i[a1,a2], i[a]] <--> [i(a1,a2), i(a)]

%[["i", "a1", "a2"],["i","a"],["i"]] --> [i(a1,a2), i(a), i]
parse(List, Program) :-
    nonvar(List),
    to_atoms(List, Flat),
    to_functors(Flat,Program), !.

%[["i", "a1", "a2"]["i","a"]] <-- [i(a1,a2), i(a)]
parse(List, Program) :-
    nonvar(Program),
    to_functors(Flat, Program),
    to_atoms(List, Flat), !.

% [["i", "a1", "a2"]["i","a"]] <--> [[i,[a1,a2]], [i,[a]]]
to_atoms([], []).

to_atoms([S|Ss], [A|As]) :-
    phrase(instruction(A), S),
    to_atoms(Ss, As).

to_functors([], []).
% [[i, [a1, a2]], [i, [a]] <--> [i(a1,a2), i(a)]
to_functors([X|Xs], [Y|Ys]) :-
    to_functor(X, Y),
    to_functors(Xs, Ys).

% [i, [a1, a2]] <--> i(a1,a2)
to_functor([],[]).
to_functor([tag(T)], tag(T)):-!.
to_functor([X|Xa], Y) :-
    [A] = Xa,
    [Xa1, Xa2] = A,
    Y =.. [X, Xa1, Xa2].
to_functor([X|Xa], Y) :-
    [A] = Xa,
    [Xa1] = A,
    Y =.. [X, Xa1].
to_functor(X, Y) :-
    Y =.. X.


%%%%%%%
%Rules%
%%%%%%%

% i <-> o
% i, o = instruction
% instruction = i(type(Arg), type(Arg2))
% type = mem | imm | reg | tag
% Arg -> arg literal, Arg variable. Arg = Arg, Arg =\= Arg2.
% syntax
% rule(<name>, [<i>], [<o>]).

% operations
% operation(op(R1, R2), op, [R1, R2])
% R1 xor R2 == xor(R1, R2)

% PUSH rules
rule(g1, [push(imm(Imm)), pop(reg(Reg))], [mov(reg(Reg), imm(Imm))]).
rule(g2, [push(reg(Reg)), pop(reg(Reg2))], [mov(reg(Reg2), reg(Reg))]).
rule(g13, [push(reg(Reg)), 'ret'], [jmp(reg(Reg))]).

% MOV rules
rule(g3, [mov(mem(Mem), imm(Imm)), push(mem(Mem))], [push(imm(Imm))]).
rule(g4, [mov(mem(Mem), reg(Reg)), push(mem(Mem))], [push(reg(Reg))]).

rule(g7, [mov(mem(Mem), imm(Imm)), Opi], [Opo]):-
    operation(Opi, O, [reg(Reg),mem(Mem)]),
    operation(Opo, O, [reg(Reg),imm(Imm)]).

rule(g8, [mov(mem(Mem2), mem(Mem)), Opi], [Opo]):-
    operation(Opi, O, [reg(Reg),mem(Mem2)]),
    operation(Opo, O, [reg(Reg),mem(Mem)]).

rule(g10, [mov(mem(Mem), reg(Reg)), call(mem(Mem))], [call(reg(Reg))]).
rule(g11, [mov(mem(Mem2), mem(Mem)), call(mem(Mem2))], [call(mem(Mem))]).

rule(g12, [mov(mem(Mem), reg(Reg)), jmp(mem(Mem))], [jmp(reg(Reg))]).
rule(g14, [mov(mem(Mem2), mem(Mem)), jmp(mem(Mem2))], [jmp(mem(Mem))]).

% POP rules
rule(g5, [pop(mem(Mem2)), mov(mem(Mem), mem(Mem2))], [pop(mem(Mem))]).
rule(g6, [pop(mem(Mem)), mov(reg(Reg), mem(Mem))], [pop(reg(Reg))]).

rule(g9, [pop(mem(Mem)), push(mem(Mem))], []).
rule(f19, [pop(reg(Reg)), push(reg(Reg))], []).
rule(g15, [pop(mem(Mem)), jmp(mem(Mem))], [ret]).

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
    parse(Program, Parsed), % Parse
    rules([], Parsed, Generated), % Generate
    compare_firms(Generated, Firms, Names, Positives). % Compare

generate([],[]).
generate(Program, Result) :-
    parse(Program, Parsed), % Parse to Functors
    rules([], Parsed, New_program), % Apply rules
    parse(Result,New_program).

compare([],_,_,[]).
compare(_,[],_,[]).
compare(_,_,[],[]).
compare(Program, Firms, Names, Positives):-
    parse(Program, Parsed),
    compare_firms(Parsed, Firms, Names, Positives).

rules(Result,[], Result).

rules(Program_before,[Last], Result):-
    append(Program_before, [Last], Result).

rules(Program_before, [N1|Program_next], Result) :-
    rule(_, [N1], R),
    append(Program_before, R, New_program_before),
    rules(New_program_before, Program_next, Result).

rules(Program_before, [N1,N2|Program_next], Result) :-
    rule(_, [N1,N2], R),
    append(Program_before, R, New_program_before),
    rules(New_program_before, Program_next, Result).

rules(Program_before, [N1,N2|Program_next], Result) :-
    append(Program_before, [N1], New_program_before),
    rules(New_program_before, [N2|Program_next], Result).


/** <examples>
?- generate([["mov", "[123]", "0x6477737361702FFF"], ["push", "[123]"], ["push", "12"], ["pop", "r12"], ["push", "r12"], ["mov", "r13", "13"], ["mov", "r12", "0xFFFFFFFF6374652F"], ["xor", "rax", "rax"], ["syscall"], ["jne", "loop_read"], ["close_file:"], ["syscall"]], R)
*/
