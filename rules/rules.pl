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

% OP
op_type(cmp).
op_type(add).
op_type(sub).
op_type(and).
op_type(xor).
op_type(test).
op_type(lea).
op_type(shl).
op_type(shr).

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
    asign(P,R),!.

arg_type([P],[R]):-
    asign(P,R).

asign(P,R):-
    reg(P), R=reg(P),!.

asign(P,R):-
    imm(P), R=imm(P),!.

asign(P,R):-
    mem(P), R=mem(P),!.

asign(P,P).

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

% Output number, not atom
string_atom(S,A):-
    var(A),
    number_string(A,S),
    !.

% Force string output in hex
string_atom(S,A):-
    var(S),
    number(A),
    format(string(S), "0x~|~16R",A),
    !.

string_atom(S,A):-
    atom_string(A,S).

list_string_list_atom([],[]).

list_string_list_atom([S|Ss],[A|As]):-
    list_string_list_atom(Ss, As),
    string_atom(S,A).

matrix_string_matrix_atom([],[]).

matrix_string_matrix_atom([S|Ss],[A|As]):-
    matrix_string_matrix_atom(Ss, As),
    list_string_list_atom(S, A).

% Matrix of atoms -> Array of Functors
parser([], []).

parser([[X1, A1, A2]|Xs], [Y|Ys]) :-
    op_type(X1),
    parser(Xs, Ys),
    Y = op(X1,A1, A2), !.

parser([X|Xs], [Y|Ys]) :-
	parser(Xs, Ys),
	Y =.. X.

% Matrix of str <- Array of Functors
re_parser([], []).

re_parser([X|Xs], [Y|Ys]) :-
	re_parser(Xs, Ys),
    parser(X1,[Y]),
    nth0(0, X1,X).

% Parse to matrix of functors
parse(Program, Parsed):-
    matrix_string_matrix_atom(Program, Atom_program),
	parser(Atom_program,Parsed).

% Parse to matrix of string
re_parse(Parsed, Result):-
    re_parser(Atoms, Parsed),
    matrix_string_matrix_atom(Result, Atoms).

%%%%%%%
%Rules%
%%%%%%%

% i <-> o
% syntax
% rule(<name>, [<i>], [<o>]) :- types (reg, imm, mem)

% operations
% op(<op>, <arg1, <arg2)
% xor rax rbx <-> op(xor, rax, rbx)

% PUSH rules
rule(g1, [push(Imm), pop(Reg)], [mov(Reg, Imm)]) :- imm(Imm), reg(Reg).
rule(g2, [push(Reg), pop(Reg2)], [mov(Reg2, Reg)]) :- reg(Reg), reg(Reg2).
rule(g13, [push(Reg), 'ret'], [jmp(Reg)]) :- reg(Reg).

% MOV rules
rule(g3, [mov(Mem, Imm), push(Mem)], [push(Imm)]) :- mem(Mem), imm(Imm).
rule(g4, [mov(Mem, Reg), push(Mem)], [push(Reg)]) :- reg(Reg), mem(Mem).

rule(g7, [mov(Mem, Imm), op(_,Reg,Mem)], [op(_,Reg,Imm)]):- mem(Mem), imm(Imm), reg(Reg).
rule(g8, [mov(Mem2, Mem), op(_,Reg, Mem2)], [op(_,Reg,Mem)]) :- mem(Mem), mem(Mem2), reg(Reg).

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

compress_and_compare([],_,_,_,[]).
compress_and_compare(_,_,[],_,[]).
compress_and_compare(_,_,_,[],[]).
compress_and_compare(Program, Compressed, Firms, Names, Positives):-
	parse(Program, Parsed), % Parse to Functors
	rules([], Parsed, Compressed),% Apply rules
    re_parser(Array_program,Compressed),
    type(Array_program, Typed_program),
    parser(Typed_program, Result),
    compare_firms(Result, Firms, Names, Positives).

compress([],[]).
compress(Program, Result) :-
	parse(Program, Parsed), % Parse to Functors
	rules([], Parsed, New_program), % Apply rules
    re_parse(New_program, Result).

compare([],_,_,[]).
compare(_,[],_,[]).
compare(_,_,[],[]).
compare(Program, Firms, Names, Positives):-
    matrix_string_matrix_atom(Program, Atoms),
    type(Atoms, Typed_atoms),
    parser(Typed_atoms, Result),
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