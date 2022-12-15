from pyswip import Prolog

from internal_functions.program import Program


def init_prolog() -> Prolog:
    """
    Create Prolog instance with rules.pl loaded
    :return: Prolog instance
    """
    prolog = Prolog()
    prolog.consult("rules/rules.pl")
    return prolog


def _parse_solutions(solution) -> list[str]:
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


def compress_prolog(program: Program, prolog=None) -> list[Program]:
    """
    Execute compress query
    :param program: Program to compress
    :param prolog: Custom prolog instance
    :return: List of compressed programs
    """
    if prolog is None:
        prolog = init_prolog()
    instructions = str(program.instructions).replace("'", '"')
    query_solutions = prolog.query("compress(" + instructions + ", R)")
    new_programs = []
    for solution in query_solutions:
        solutions = _parse_solutions(solution["R"])
        for s in solutions:
            new_program = _str_to_program(s, program)
            new_programs.append(new_program)
    return new_programs


def compare_prolog(program: Program, signatures: str, names: str, prolog=None) -> set[str]:
    """
    Execute compare query
    :param program: Program to compare
    :param signatures: Content of signatures
    :param names: Names of signatures
    :param prolog: Custom prolog instance
    :return: Set of names of rules with positive match
    """
    if prolog is None:
        prolog = init_prolog()
    all_positives = set()
    instructions = str(program.instructions).replace("'", '"')
    names = str(names.replace("'", '"'))
    query_solutions = prolog.query("compare(" + instructions + ", " + signatures + ", " + names + ", Positives)")
    for solution in query_solutions:
        positives = _parse_solutions(solution["Positives"])
        positives = str(positives).replace("[", "").replace("]", "").split(", ")
        for positive in positives:
            all_positives.add(positive.replace("'", "").replace('"', ""))
    return all_positives
