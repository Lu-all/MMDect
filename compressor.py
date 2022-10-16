from Program import Program
from RulesHandler import RulesHandler


def compress_program(program: Program) -> Program:
    """
    Compress program using metamorphic rules
    :param program: program to compress
    :return: None
    """

    rule_handler = RulesHandler()
    rule_handler.apply_all_rules_global(program)
    return program
