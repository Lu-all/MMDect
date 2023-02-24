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

% mem('Mem') --> ["[Mem]"]
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
operation(Operand, Args):-
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

%%%%%%%%%
%Compare%
%%%%%%%%%

compare_firm([],_,_,[]).

compare_firm([PLine|_Program], [Line], Name, Positive):-
    check(PLine, Line),
    Positive = [Name].

compare_firm([PLine|Program], [Line|Lines],Name, Positive):-
    check(PLine, Line),
    compare_firm(Program, Lines, Name, Positive).

compare_firm(_, _, _, []).

check([],[]).
check(L, M):-
    L = M.

%%%%%%%
%Parse%
%%%%%%%  

% Parse flow:
% [["i", "a1", "a2"]["i","a"]] <--> [i[a1,a2], i[a]] <--> [i(a1,a2), i(a)]

%[["i", "a1", "a2"],["i","a"],["i"]] --> [i(a1,a2), i(a), i]
parser(List, Program) :-
    nonvar(List),
    to_atoms(List, Flat),
    to_functors(Flat,Program), !.

%[["i", "a1", "a2"]["i","a"]] <-- [i(a1,a2), i(a)]
parser(List, Program) :-
    nonvar(Program),
    to_functors(Flat, Program),
    to_atoms(List, Flat), !.

% [["i", "a1", "a2"]["i","a"]] <--> [[i,[a1,a2]], [i,[a]]]
to_atoms([], []).

to_atoms([Co|[]], [Cr|[]]) :-
    (atom(Cr);string(Co);
    (nonvar(Cr),functor(Cr,tag, 1))),
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
%xor -> xorr
  
% PUSH rules
rule(g1, [push(imm(Imm)), pop(reg(Reg))], [mov(reg(Reg), imm(Imm))]).
rule(g2, [push(reg(Reg)), pop(reg(Reg2))], [mov(reg(Reg2), reg(Reg))]).
rule(g13, [push(reg(Reg)), 'ret'], [jmp(reg(Reg))]).

% MOV rules
rule(g3, [mov(mem(Mem), imm(Imm)), push(mem(Mem))], [push(imm(Imm))]).
rule(g4, [mov(mem(Mem), reg(Reg)), push(mem(Mem))], [push(reg(Reg))]).

rule(g7, [mov(mem(Mem), imm(Imm)), Opi], Opo):-
    operation(Opi,[reg(Reg),mem(Mem)]),
    operation(Opo,[reg(Reg),imm(Imm)]).
    
rule(g8, [mov(mem(Mem2), mem(Mem)), Opi], Opo):-
    operation(Opi,[reg(Reg),mem(Mem2)]),
    operation(Opo,[reg(Reg),mem(Mem)]).
  
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

compress_and_compare([],_,_,_,[]).
compress_and_compare(_,_,[],_,[]).
compress_and_compare(_,_,_,[],[]).
compress_and_compare(Program, Compressed, Firms, Names, Positives):-
    parser(Program, Parsed), % Parse
    rules([], Parsed, Compressed), % Compress
    compare_firms(Compressed, Firms, Names, Positives). % Compare

compress([],[]).
compress(Program, Result) :-
    parser(Program, Parsed), % Parse to Functors
    rules([], Parsed, New_program), % Apply rules
    parser(Result,New_program).

compare([],_,_,[]).
compare(_,[],_,[]).
compare(_,_,[],[]).
compare(Program, Firms, Names, Positives):-
    parser(Program, Parsed),
    compare_firms(Parsed, Firms, Names, Positives).
  
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
  
compare_firms([], _,_, []).
compare_firms(_,[],_,[]).


compare_firms([Line], [Firm|Firms], [Name|Names], Positives):-
    compare_firms([Line], Firms, Names, New_positives),
    compare_firm(Line, Firm, Name, Positive),
    append(New_positives, Positive, Positives),!.

compare_firms([Line|Program], [Firm|Firms], [Name|Names], Positives):-
    compare_firms(Program, [Firm|Firms], [Name|Names], New_positives_a),
    compare_firms([Line|Program], Firms, Names, New_positives_b),
    append(New_positives_a, New_positives_b, New_positives),
    compare_firm([Line|Program], Firm, Name, Positive),
    append(New_positives, Positive, Positives),!.
  

%%%%%%%%%%
%Examples%
%%%%%%%%%%

etc_shadow_sign([
    mov(reg(Reg),imm(0x6477737361702FFF)),
    shr(reg(Reg),imm(8)),
    push(reg(Reg)),
    mov(reg(Reg),imm(0xFFFFFFFF6374652F)),
    shl(reg(Reg),imm(32)),
    push(reg(Reg))
    ]).
  
compressed_test([
    mov(mem('123'), imm(_Imm)),
    push(mem('123')),
    push(imm(_Imm2)),
    mov(reg(r13), imm(_Imm3))
    ]).
  
    
original_test([
    mov(mem('123'),
    imm(7239381865414537215)),
    push(mem('123')),
    push(imm(12))
    ]).
  
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
  
  
/** <examples>
 ?- test_special(L), parser(L, P), parser(L1,P).
 ?- test(P), compress(P, Result).
 ?- test_long(P), etc_shadow_sign(R),compress_and_compare(P,C, [R], ['etc/shadow'], Positives).
 ?- test(P), etc_shadow_sign(R1), test_sign(R2), compress_and_compare(P, R, [R1,R2], ['etc/shadow','test'], Positives).
 ?- test(P), etc_shadow_sign(R1), test_sign(R2), test_cmp(R3), compress_and_compare(P, R, [R1,R2,R3], ['etc/shadow','test', 'original'], Positives), \+length(Positives, 0).
 */