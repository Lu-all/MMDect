from typing import List, Set

from internal_functions.IOHandler import get_prolog_signatures, get_regex_signatures
from internal_functions.colored import prints, print_pass
from internal_functions.program import Program
from internal_functions.utils import reformat_signatures
from modules.prolog.prologHandler import generate_prolog, compare_prolog
from modules.python.regexHandler import compare_python
from modules.python.rulesHandler import RulesHandler


def generate_program(program: Program, python_exec: str, dcg_prolog=True, tag_replacement=False, silent=False) \
        -> List[Program]:
    """
    Generate new programs using metamorphic rules
    :param python_exec: Generate in Python
    :param dcg_prolog: True to use DCG, False to use classic Prolog
    :param tag_replacement: True to use experimental tag substitution
    :param program: program to generate
    :param silent: True to hide output
    :return: List of programs generated
    """
    programs = []
    if python_exec in ["both", "generate"]:
        prints("\t[+] Generating in Python", silent)
        rule_handler = RulesHandler()
        rule_handler.generate_python(program, experimental_rules=tag_replacement)
        programs.append(program)
    else:
        prints("\t[+] Generating in Prolog", silent)
        programs = generate_prolog(program=program, dcg=dcg_prolog)
    return programs


def compare_program(program: Program, path: str, python_exec: str, both_signatures=False, dcg_prolog=True,
                    silent=False, iteration=0) -> Set[str]:
    """
    Compare program with given signatures
    :param program: Instructions to compare
    :param path: Path to signatures
    :param python_exec: Compare in Python
    :param both_signatures: Enables both comparison modes (Python and Prolog)
    :param dcg_prolog: True to use DCG, False to use classic Prolog
    :param silent: True to hide output
    :param iteration: Iteration of program generated
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
        positives = compare_prolog(program=program, signatures=signatures, names=str(names), dcg=dcg_prolog)
        names, signatures = get_regex_signatures(path)
        instructions = str(program.instructions).replace('"', "").replace("[[", "[").replace("]]", "]")
        positives_regex = compare_python(program=instructions, signatures=signatures, names=names)
        positives = positives.union(positives_regex)
    else:
        if python_exec in ["both", "compare"]:
            names, signatures = get_regex_signatures(path)
            instructions = str(program.instructions).replace('"', "").replace("[[", "[").replace("]]", "]")
            positives = compare_python(program=instructions, signatures=signatures, names=names)
        else:
            names, signatures = get_prolog_signatures(path)
            signatures = reformat_signatures(signatures)
            prints("\t[+] Comparing in Prolog", silent)
            positives = compare_prolog(program=program, signatures=signatures, names=str(names), dcg=dcg_prolog)
    if len(positives) == 0:
        print_pass("\tNo positives found in iteration n:" + str(iteration), silent)
    else:
        print_pass("\tPositives iteration n:" + str(iteration) + " - " + str(positives), silent)
    return positives


def generate_and_compare_program(program: Program, path: str, python_exec: str, both_signatures=False, dcg_prolog=True,
                                 tag_replacement=False, silent=False) -> \
        tuple[List[Program], Set[str]]:
    """
    Generate and compare program with given signatures
    :param program: program to generate
    :param path: Path to signatures
    :param python_exec: Compare or compare in Python
    :param both_signatures: Enables both comparison modes (Python and Prolog)
    :param dcg_prolog: True to use DCG, False to use classic Prolog
    :param tag_replacement: True to use experimental tag substitution
    :param silent: True to hide output
    :return: List of generated programs and set of names of rules with positive match
    """
    positives = set()
    programs = generate_program(program=program, tag_replacement=tag_replacement, python_exec=python_exec,
                                dcg_prolog=dcg_prolog)
    programs.append(program)
    iteration = 0
    for new_program in programs:
        iteration = iteration + 1
        if len(positives) == 0:
            positives = compare_program(program=new_program, path=path, python_exec=python_exec,
                                        both_signatures=both_signatures, silent=silent,
                                        iteration=iteration, dcg_prolog=dcg_prolog)
        else:
            positives = positives.union(
                compare_program(program=new_program, path=path, python_exec=python_exec,
                                both_signatures=both_signatures,
                                silent=silent, iteration=iteration, dcg_prolog=dcg_prolog))
    programs.remove(program)
    return programs, positives
