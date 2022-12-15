import re

from internal_functions.program import Program


def _get_registers():
    return ["eax", "ebx", "ecx", "edx", "esp", "ebp", "esi", "edi", "rax", "rbx", "rip", "rcx", "rdx", "rsp",
            "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]


def _get_operations():
    return ["pop", "push", "nop", "mov", "cmp", "add", "sub", "and", "xor", "test", "lea", "shr", "shl"]


def _get_conditional_branch():
    return ["jz", "je", "jne", "jl", "jle", "jg", "jge"]


def _get_branch():
    return ["jmp", "call", "syscall"]


def _get_return_type():
    return ["ret"]


def is_instruction(possible_instruction: str) -> bool:
    """
    Return True if possible_instruction is an accepted instruction
    :param possible_instruction: literal line to test
    :return: True if it is an accepted instruction, False if not
    """
    return is_operation(possible_instruction) or is_branch(
        possible_instruction) or is_conditional_branch(possible_instruction) or is_return(
        possible_instruction)


def is_operation(possible_operation: str) -> bool:
    """
    Check if instruction is an operation
    :param possible_operation: instruction to test
    :return: True if is operation, False if not
    """
    return possible_operation in _get_operations()


def is_conditional_branch(possible_conditional_branch: str) -> bool:
    """
    Check if instruction is a conditional_branch
    :param possible_conditional_branch: instruction to test
    :return: True if is operation, False if not
    """
    return possible_conditional_branch in _get_conditional_branch()


def is_branch(possible_branch: str) -> bool:
    """
    Check if instruction is a branch
    :param possible_branch: instruction to test
    :return: True if is operation, False if not
    """
    return possible_branch in _get_branch()


def is_return(possible_return: str) -> bool:
    """
    Check if instruction is a return instruction
    :param possible_return: instruction to test
    :return: True if is operation, False if not
    """
    return possible_return in _get_return_type()


def is_register(operand: str) -> bool:
    """
    Check if operand is a register
    :param operand: operand to check
    :return: True if operand, False if not
    """
    return operand in _get_registers()


def is_memory_address(operand) -> bool:
    """
    Check if operand is a memory address
    :param operand: operand to check
    :return: True if memory address, False if not
    """
    return '[' in str(operand)


def is_tag(program: Program, operand: str) -> bool:
    """
    Check if operand is a tag
    :param operand: operand to check
    :param program: program where the operand belongs
    :return: True if tag, False if not
    """
    return str(operand) in program.tags


def is_immediate(program: Program, operand: str) -> bool:
    """
    Check if operand is an immediate
    :param operand: operand to check
    :param program: program where the operand belongs
    :return: True if immediate, False if not
    """
    return not is_register(operand) and not is_memory_address(operand) and not is_tag(program,
                                                                                      operand)


def reformat_signatures(signatures: list[list[str]]) -> str:
    """
    Reformat signatures to evade regex and string shenanigans
    :param signatures: Signatures that may fail in comparation
    :return: Signatures with evaded regex and string shenanigans
    """
    new_signatures = []
    for content in signatures:
        new_signatures.append(content)
    content = re.sub(r'\'\[(\d*)]\'', r'meminit \1 memfin', str(new_signatures))
    content = content.replace('"', "").replace("'", "").replace(",,", ",")
    new_signatures = re.sub(r'meminit (\d*) memfin', r"'[\1]'", content)
    return str(new_signatures)
