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

is_matrix(Matrix):-
    nonvar(Matrix), nth0(0,Matrix,M1), nth0(0,M1,_).

list_string_list_atom([],[]).

list_string_list_atom([S|Ss],[A|As]):-
    list_string_list_atom(Ss, As),
    atom_string(A,S).

matrix_string_matrix_atom([],[]).

matrix_string_matrix_atom([S|Ss],[A|As]):-
    matrix_string_matrix_atom(Ss, As),
    list_string_list_atom(S, A).

list_matrix_string_list_matrix_atom([],[]).

list_matrix_string_list_matrix_atom([S|Ss],[A|As]):-
    list_matrix_string_list_matrix_atom(Ss, As),
        (
                is_matrix(S)
        ;
                is_matrix(A)
        ),
    matrix_string_matrix_atom(S, A), !.

list_matrix_string_list_matrix_atom([S|Ss],[A|As]):-
    list_matrix_string_list_matrix_atom(Ss, As),
    list_string_list_atom(S, A).


% Matrix of str <-> Array of Functors
parser([], []).

parser([X|Xs], [Y|Ys]) :-
	parser(Xs, Ys),
	Y =.. X.

re_parser([], []).

re_parser(X, Y):-
    length(Y, 1),
    nth0(0,Y,Y1),
	parser(X1, Y1),
    append([], [X1], X).

re_parser([X|Xs], [Y|Ys]) :-
	re_parser(Xs, Ys),
    parser(X,Y).

% Array is input
% Once Tail is deduced, Head & Body are resolved
cut(Head,Body,Tail, Array) :-
    append(Extra, Tail, Array),
    append(Head,Body,Extra),
    length(Body, L), L > 0.

% Array is output
% Once Head & Body are merged, Tail is merged too.
substitute(Head,Body,Tail, Array) :-
    append(Head,Body,Extra),
    append(Extra, Tail, Array).

filter_result([], []).

filter_result([R|Not_filtered_result], Result):-
    member(R, Not_filtered_result),
    filter_result(Not_filtered_result, Result).

filter_result([R|Not_filtered_result], Result):-
    filter_result(Not_filtered_result, New_result),
    append(New_result, [R], Result).

all_in_one([], []).
all_in_one([L|Bag], _Unified):-
    member(L, Bag),
    all_in_one(Bag, _New_unified).

all_in_one([L|Bag], Unified):-
    all_in_one(Bag, New_unified),
    filter_result(L, LF),
    append(New_unified, LF, Unified).

% Daniel Lyons
without_last([_], []).
without_last([X|Xs], [X|WithoutLast]) :-
    without_last(Xs, WithoutLast).

get_content(Program_before, N, Content) :-
    length(Program_before, LP),
	((
    	LP < 1, Content = N
     )
     ;
     (
		nth1(LP,Program_before,C1), append([C1],[N],Content)
     )).

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
re_parse(Result, Parsed):-
    re_parser(Atoms, Parsed),
    list_matrix_string_list_matrix_atom(Result, Atoms).

format_solutions(Not_formatted, Formatted):-
    all_in_one(Not_formatted,Not_filtered_result),
    filter_result(Not_filtered_result, Formatted).

all_main(Program, Result) :-
    parse(Program, Parsed), % Parse to Functors
	setof(R, rules([], Parsed, [], R), Bag), % Apply rules
    all_in_one(Bag, Not_filtered_result),
    filter_result(Not_filtered_result, Result), !. % Format to re_parse

all_main_limit(Program, Result, Limit) :-
    parse(Program, Parsed), % Parse to Functors
	setof(R, limit(Limit,rules([], Parsed, [], R)), Bag), % Apply rules
    all_in_one(Bag, Not_filtered_result),
    filter_result(Not_filtered_result, Result), !. % Format to re_parse

main(Program, Result) :-
	parse(Program, Parsed), % Parse to Functors
	rules([], Parsed, [], Bag), % Apply rules
    filter_result(Bag, Result).

apply_rule(Program_before, Result, Program_next, Applied_before, New_program_before, New_applied_before) :-
      without_last(Program_before, Pbl),
      substitute(Pbl, Result, Program_next, New_applied),
      ((
          member(New_applied, Applied_before), New_applied_before = Applied_before
      )
      ;
      (
          append(Applied_before, [New_applied], New_applied_before)
      )),
      append(Pbl, Result, New_program_before).

ignore_rule(Program_before, N, Program_next, Applied_before, New_program_before, New_applied_before) :-
	substitute(Program_before, [N], Program_next, New_applied),
    ((
        member(New_applied, Applied_before),
        New_applied_before = Applied_before
	)
	;
	(
        append(Applied_before, [New_applied], New_applied_before)
	)),
    append(Program_before, [N], New_program_before).

rules(_,[],Applied_before, R) :-
    R = Applied_before.

rules(Program_before, [N|Program_next], Applied_before, R) :-
	get_content(Program_before, N, Content),
    (
		rule(_, Content, Result),
        (
        		%Apply rule
        		apply_rule(Program_before, Result, Program_next, Applied_before, New_program_before, New_applied_before);
        		%Ignore rule
            	ignore_rule(Program_before, N, Program_next, Applied_before, New_program_before, New_applied_before)
        ),
        rules(New_program_before, Program_next, New_applied_before, R)
	)
    ;
    (
		% Cannot Apply rule
		ignore_rule(Program_before, N, Program_next, Applied_before, New_program_before, New_applied_before),
		rules(New_program_before, Program_next, New_applied_before, R)
    ).


/** <examples>
?- test(P), main(P, _Result), re_parse(R, _Result).
*/
