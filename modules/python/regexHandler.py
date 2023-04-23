import re
from typing import Set, List

from internal_functions.colored import prints


def compare_python(program: str, signatures: List[List[str]], names: List[str], silent=False) \
        -> Set[str]:
    """
    Compare instructions to signatures provided in Python and return matches
    :param program: instructions to compare
    :param signatures: Content of signatures
    :param names: Names of signatures
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
