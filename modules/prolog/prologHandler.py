from typing import List, Set

from pyswip import Prolog

from internal_functions.program import Program


def init_prolog(dcg=True) -> Prolog:
    """
    Create Prolog instance with dcg_rules.pl loaded
    :param dcg: True to use DCG, False to use classic Prolog
    :return: Prolog instance
    """
    prolog = Prolog()
    if not dcg:
        prolog.consult("modules/prolog/rules.pl")
    else:
        prolog.consult("modules/prolog/dcg_rules.pl")
    return prolog


def _parse_solutions(solution) -> List[str]:
    return str(solution).replace("[b'", "['").replace(", b'", ", '").replace("[[[", "[[").replace(
        "]]]", "]]").replace("]], [[", "]]#[[").split('#')


def _str_to_program(s, program) -> Program:
    new_program = program.copy()
    new_program.instructions = []
    new_instructions = s.replace("[[", "[").replace("]]", "]").replace("], [", "],\t[").split(",\t")
    for instruction in new_instructions:
        instruction = instruction.replace("['", "").replace(" ", "").replace("']", "").replace("'", ""). \
            split(",")
        new_program.add_instruction(instruction)
    return new_program


def generate_prolog(program: Program, dcg=True, prolog=None) -> List[Program]:
    """
    Execute generate query
    :param program: Base program to generate versions of
    :param dcg: True to use DCG, False to use classic Prolog
    :param prolog: Custom prolog instance
    :return: List of generated programs
    """
    if prolog is None:
        prolog = init_prolog(dcg=dcg)
    instructions = str(program.instructions).replace("'", '"')
    query_solutions = prolog.query("generate(" + instructions + ", R)")
    new_programs = []
    for solution in query_solutions:
        solutions = _parse_solutions(solution["R"])  # type: ignore
        for s in solutions:
            new_program = _str_to_program(s, program)
            new_programs.append(new_program)
    return new_programs


def compare_prolog(program: Program, signatures: str, names: str, dcg=True, prolog=None) -> Set[str]:
    """
    Execute compare query
    :param program: Program to compare
    :param signatures: Content of signatures
    :param names: Names of signatures
    :param dcg: True to use DCG, False to use classic Prolog
    :param prolog: Custom prolog instance
    :return: Set of names of rules with positive match
    """
    if prolog is None:
        prolog = init_prolog(dcg)
    all_positives = set()
    instructions = str(program.instructions).replace("'", '"')
    names = str(names.replace("'", '"'))
    query_solutions = prolog.query("compare(" + instructions + ", " + signatures + ", " + names + ", Positives)")
    for solution in query_solutions:
        positives = _parse_solutions(solution["Positives"])  # type: ignore
        if positives[0] != '[]':
            positives = str(positives).replace("[", "").replace("]", "").split(", ")
            for positive in positives:
                all_positives.add(positive.replace("'", "").replace('"', ""))
    return all_positives
