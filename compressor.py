from Program import Program
from RulesHandler import RulesHandler
from prologRules import apply_rules


def compress_program(program: Program, python_exec: bool, tag_replacement: bool) -> list[Program]:
    """
    Compress program using metamorphic rules
    :param python_exec:
    :param tag_replacement:
    :param program: program to compress
    :return: None
    """
    programs = []
    if python_exec:
        rule_handler = RulesHandler()
        rule_handler.apply_all_rules_global(program, experimental_rules=tag_replacement)
        programs.append(program)
    else:
        programs = apply_rules(program)
    return programs
