import re

from internal_functions.program import Program
from internal_functions.utils import reformat_signatures
from rules.prologHandler import compress_prolog, compare_prolog
from rules.pythonRulesHandler import RulesHandler


def get_all_coincidences(program, signatures, names, experimental_rules) -> set[str]:
    """
    Compare instructions to signatures provided in Python and return matches.
    :param program: instructions to compare
    :param signatures: Content of signatures
    :param names: Names of signatures
    :param experimental_rules: True to use experimental tag substitution
    :return: Set of names of rules with positive match
    """
    positives = set()
    for i in range(0, len(names)):
        for line in signatures[i]:
            match = re.search(line.replace('"', "").replace("[[", "[").replace("]]", "]"), program)
            if match is not None:
                positives.add(names[i])
    return positives


def compress_program(program: Program, python_exec: int, tag_replacement=False) -> list[Program]:
    """
    Compress program using metamorphic rules
    :param python_exec: Compress in Python
    :param tag_replacement: True to use experimental tag substitution
    :param program: program to compress
    :return: List of programs compressed
    """
    programs = []
    if python_exec == 1 or python_exec > 2:
        rule_handler = RulesHandler()
        rule_handler.apply_all_rules_global(program, experimental_rules=tag_replacement)
        programs.append(program)
    else:
        programs = compress_prolog(program)
    return programs


def compare_program(program: Program, signatures: list[list[str]], names: list[str], python_exec: int,
                    tag_replacement=False) -> set[str]:
    """
    Compare program with given signatures
    :param program: instructions to compare
    :param signatures: Content of signatures
    :param names: Names of signatures
    :param python_exec: Compare in Python
    :param tag_replacement: True to use experimental tag substitution
    :return: Set of names of rules with positive match
    """
    if python_exec >= 2:
        instructions = str(program.instructions).replace('"', "").replace("[[", "[").replace("]]", "]")
        positives = get_all_coincidences(program=instructions, signatures=signatures, names=names,
                                         experimental_rules=tag_replacement)
    else:
        signatures = reformat_signatures(signatures)
        positives = compare_prolog(program, signatures, str(names))
    return positives


def compress_and_compare_program(program: Program, signatures, names: list[str], python_exec: int,
                                 tag_replacement=False) -> \
        list[list[Program], set[str]]:
    """
    Compress and compare program with given signatures
    :param program: program to compress
    :param signatures: Content of signatures
    :param names: Names of signatures
    :param python_exec: Compare or compare in Python
    :param tag_replacement: True to use experimental tag substitution
    :return: List of compressed programs and set of names of rules with positive match
    """
    programs = []
    positives = set()
    if python_exec == 1 or python_exec > 2:
        rule_handler = RulesHandler()
        rule_handler.apply_all_rules_global(program, experimental_rules=tag_replacement)
        programs.append(program)
    else:
        signatures = reformat_signatures(signatures)
        programs = compress_prolog(program)
    if python_exec >= 2:
        for new_program in programs:
            positives.add(
                compare_program(program=new_program, signatures=signatures, names=names, python_exec=python_exec))
    else:
        for new_program in programs:
            new_positives = compare_prolog(new_program, signatures, str(names))
            for positive in new_positives:
                positives.add(positive)
    return [programs, positives]
