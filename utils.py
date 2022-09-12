from Program import Program


def get_registers():
    return ["eax", "ebx", "ecx", "edx", "esp", "ebp", "esi", "edi", "rax", "rbx", "rip", "rcx", "rdx", "rsp",
            "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]


def get_operations():
    return ["pop", "push", "nop", "mov", "cmp", "add", "sub", "and", "xor", "test", "lea", "shr", "shl"]


def get_conditional_branch():
    return ["jz", "je", "jne", "jl", "jle", "jg", "jge"]


def get_branch():
    return ["jmp", "call", "syscall"]


def get_return_type():
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
    return possible_operation in get_operations()


def is_conditional_branch(possible_conditional_branch: str) -> bool:
    """
    Check if instruction is a conditional_branch
    :param possible_conditional_branch: instruction to test
    :return: True if is operation, False if not
    """
    return possible_conditional_branch in get_conditional_branch()


def is_branch(possible_branch: str) -> bool:
    """
    Check if instruction is a branch
    :param possible_branch: instruction to test
    :return: True if is operation, False if not
    """
    return possible_branch in get_branch()


def is_return(possible_return: str) -> bool:
    """
    Check if instruction is a return instruction
    :param possible_return: instruction to test
    :return: True if is operation, False if not
    """
    return possible_return in get_return_type()


def is_register(operand):
    """
    Check if operand is a register
    :param operand: operand to check
    :return: True if operand, False if not
    """
    return operand in get_registers()


def is_memory_address(operand):
    """
    Check if operand is a memory address
    :param operand: operand to check
    :return: True if memory address, False if not
    """
    return '[' in str(operand)


def is_tag(program: Program, operand):
    """
    Check if operand is a tag
    :param operand: operand to check
    :param program: program where the operand belongs
    :return: True if tag, False if not
    """
    return operand in program.tags


def is_immediate(program: Program, operand):
    """
    Check if operand is an immediate
    :param operand: operand to check
    :param program: program where the operand belongs
    :return: True if immediate, False if not
    """
    return not is_register(operand) and not is_memory_address(operand) and not is_tag(program,
                                                                                      operand)
