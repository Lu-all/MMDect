import re
from typing import List, Set

from internal_functions.IOHandler import get_prolog_signatures, get_regex_signatures
from internal_functions.colored import prints
from internal_functions.program import Program
from internal_functions.utils import reformat_signatures
from rules.prologHandler import compress_prolog, compare_prolog
from rules.pythonRulesHandler import RulesHandler


def get_all_coincidences(program: str, signatures: List[List[str]], names: List[str],
                         experimental_rules=False, silent=False) \
        -> Set[str]:
    """
    Compare instructions to signatures provided in Python and return matches
    :param program: instructions to compare
    :param signatures: Content of signatures
    :param names: Names of signatures
    :param experimental_rules: True to use experimental tag substitution
    :param silent: True to hide output
    :return: Set of names of rules with positive match
    """
    prints("\t[+] Comparing in Python", silent)
    positives = set()
    for i in range(0, len(names)):
        for line in signatures[i]:
            match = re.search(line.replace('"', "").replace("[[", "[").replace("]]", "]"), program)
            if match is not None:
                positives.add(names[i])
    return positives


def compress_program(program: Program, python_exec: str, tag_replacement=False, silent=False) -> List[Program]:
    """
    Compress program using metamorphic rules
    :param python_exec: Compress in Python
    :param tag_replacement: True to use experimental tag substitution
    :param program: program to compress
    :param silent: True to hide output
    :return: List of programs compressed
    """
    programs = []
    if python_exec in ["both", "compress"]:
        prints("\t[+] Compressing in Python", silent)
        rule_handler = RulesHandler()
        rule_handler.apply_all_rules_global(program, experimental_rules=tag_replacement)
        programs.append(program)
    else:
        prints("\t[+] Compressing in Prolog", silent)
        programs = compress_prolog(program)
    return programs


def compare_program(program: Program, path: str, python_exec: str, both_signatures=False,
                    tag_replacement=False, silent=False, iteration=0) -> Set[str]:
    """
    Compare program with given signatures
    :param program: Instructions to compare
    :param path: Path to signatures
    :param python_exec: Compare in Python
    :param both_signatures: Enables both comparison modes (Python and Prolog)
    :param tag_replacement: True to use experimental tag substitution
    :param silent: True to hide output
    :param iteration: Iteration of program compressed
    :return: Set of names of rules with positive match
    """
    if iteration > 0:
        prints("[-] Comparing v." + str(iteration), silent)
    else:
        prints("[-] Comparing", silent)
    if both_signatures:
        names, signatures = get_prolog_signatures(path)
        signatures = reformat_signatures(signatures)
        prints("\t[+] Comparing in Prolog", silent)
        positives = compare_prolog(program, signatures, str(names))
        names, signatures = get_regex_signatures(path)
        instructions = str(program.instructions).replace('"', "").replace("[[", "[").replace("]]", "]")
        positives_regex = get_all_coincidences(program=instructions, signatures=signatures, names=names,
                                               experimental_rules=tag_replacement)
        positives = positives.union(positives_regex)
    else:
        if python_exec in ["both", "compare"]:
            names, signatures = get_regex_signatures(path)
            instructions = str(program.instructions).replace('"', "").replace("[[", "[").replace("]]", "]")
            positives = get_all_coincidences(program=instructions, signatures=signatures, names=names,
                                             experimental_rules=tag_replacement)
        else:
            names, signatures = get_prolog_signatures(path)
            signatures = reformat_signatures(signatures)
            prints("\t[+] Comparing in Prolog", silent)
            positives = compare_prolog(program, signatures, str(names))
    return positives


def compress_and_compare_program(program: Program, path: str, python_exec: str, both_signatures=False,
                                 tag_replacement=False, silent=False) -> \
        tuple[List[Program], Set[str]]:
    """
    Compress and compare program with given signatures
    :param program: program to compress
    :param path: Path to signatures
    :param python_exec: Compare or compare in Python
    :param both_signatures: Enables both comparison modes (Python and Prolog)
    :param tag_replacement: True to use experimental tag substitution
    :param silent: True to hide output
    :return: List of compressed programs and set of names of rules with positive match
    """
    positives = set()
    programs = compress_program(program, python_exec, tag_replacement)
    iteration = 0
    for new_program in programs:
        iteration = iteration + 1
        if len(positives) == 0:
            positives = compare_program(new_program, path, python_exec, both_signatures,
                                        tag_replacement, silent, iteration)
        else:
            positives = positives.union(
                compare_program(new_program, path, python_exec, both_signatures, tag_replacement, silent, iteration))
    return programs, positives
