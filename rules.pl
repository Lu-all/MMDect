%%%%%%%%%%
%Examples%
%%%%%%%%%%
 
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
parser([], []).

parser([X|Xs], [Y|Ys]) :-
	parser(Xs, Ys),
	Y =.. X.


% Array is input
% Once Tail is deduced, Head & Body are resolved
cut(Head,Body,Tail, Array) :- 
    append(Extra, Tail, Array),
    append(Head,Body,Extra).

% Array is output
% Once Head & Body are merged, Tail is merged too.
substitute(Head,Body,Tail, Array) :- 
    append(Head,Body,Extra),
    append(Extra, Tail, Array).

% Function by Daniel Lyons
without_last([_], []).
without_last([X|Xs], [X|WithoutLast]) :- 
    without_last(Xs, WithoutLast).
	
%%%%%%%
%Rules%
%%%%%%%

rule(1, [push(Imm), pop(Reg)], [mov(Reg, Imm)]) :- imm(Imm), reg(Reg).
rule(3, [mov(Mem, Imm), push(Mem)], [push(Imm)]) :- mem(Mem), imm(Imm).

%%%%%%
%Main%
%%%%%%
    
main(Result) :-
	test(Program),
	parser(Program,Parsed), % Parse to Functors
	rules([], Parsed, [], Result). % Apply rules
    parser(Result,Applied). % Parse to matrix
	

get_content(Program_before, N, Content) :-
    length(Program_before, LP),
	((
    	LP < 1, Content = N
    )
    ;   
    (   
		nth1(LP,Program_before,C1), append([C1],[N],Content)
    )).
    
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
        (    %Applies rule
              apply_rule(Program_before, Result, Program_next, Applied_before, New_program_before, New_applied_before),
              rules(New_program_before, Program_next, New_applied_before, R)
        )
        ;
       	(   %Ignores rule
              ignore_rule(Program_before, N, Program_next, Applied_before, New_program_before, New_applied_before),
              rules(New_program_before, Program_next, New_applied_before, R)
        )
        )
	)
    ;   
    (
		% Cannot Apply rule
		ignore_rule(Program_before, N, Program_next, Applied_before, New_program_before, New_applied_before),
		rules(New_program_before, Program_next, New_applied_before, R)
    ).
    
    