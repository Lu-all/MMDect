%%%%%%%%%%
%Examples%
%%%%%%%%%%
 
 test([
["mov", "[123]","0x6477737361702FFF"],
["push", "[123]"],
["pop", "[123]"],   
["push", "12"],
["pop", "r12"],
["mov", "r13", "13"],
["mov", "r12", "0xFFFFFFFF6374652F"],
["xor", "rax", "rax"]
]).

test_special([
["mov", "[123]","0x6477737361702FFF"],
["push", "[123]"],
["pop", "[123]"],
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
    nonvar(X), atom_number(X, _).
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

list_string_list_atom([],[]).

list_string_list_atom([S|Ss],[A|As]):-
    list_string_list_atom(Ss, As),
    atom_string(A,S).

matrix_string_matrix_atom([],[]).

matrix_string_matrix_atom([S|Ss],[A|As]):-
    matrix_string_matrix_atom(Ss, As),
    list_string_list_atom(S, A).

% Matrix of str <-> Array of Functors
parser([], []).

parser([X|Xs], [Y|Ys]) :-
	parser(Xs, Ys),
	Y =.. X.

re_parser([], []).

re_parser([X|Xs], [Y|Ys]) :-
	re_parser(Xs, Ys),
    parser(X1,[Y]),
    nth0(0, X1,X).

filter_result([], []).

filter_result([R|Not_filtered_result], Result):-
    member(R, Not_filtered_result),
    filter_result(Not_filtered_result, Result).

filter_result([R|Not_filtered_result], Result):-
    filter_result(Not_filtered_result, New_result),
    append(New_result, [R], Result).

%%%%%%%
%Rules%
%%%%%%%

rule(1, [push(Imm), pop(Reg)], [mov(Reg, Imm)]) :- imm(Imm), reg(Reg).
rule(3, [mov(Mem, Imm), push(Mem)], [push(Imm)]) :- mem(Mem), imm(Imm).
rule(9, [push(Mem), pop(Mem)], []) :- mem(Mem).

%%%%%%
%Main%
%%%%%%

% Parse to matrix of functors
parse(Program, Parsed):-
    matrix_string_matrix_atom(Program, Atom_program),
	parser(Atom_program,Parsed).

% Parse to matrix of string
re_parse(Parsed, Result):-
    re_parser(Atoms, Parsed),
    matrix_string_matrix_atom(Result, Atoms).

main(Program, Result) :-
	parse(Program, Parsed), % Parse to Functors
	rules([], Parsed, New_program), % Apply rules
    re_parse(New_program, Result).

rules(Program_before,[Last], Result):-
    append(Program_before, [Last], Result).

rules(Result,[], Result):- !.

rules(Program_before, [N1,N2|Program_next], Result) :-
	rule(_, [N1,N2], R),
	append(Program_before, R, New_program_before),
    rules(New_program_before, Program_next, Result).

rules(Program_before, [N1,N2|Program_next], Result) :-
    append(Program_before, [N1], New_program_before),
    rules(New_program_before, [N2|Program_next], Result).


/** <examples>
?- test(P), main(P, _Result), re_parse(R, _Result).
?- test(P), all_main(P, _Result), re_parse(R, _Result).
?- test(P), all_main_limit(P, _Result), re_parse(R, _Result).
*/
