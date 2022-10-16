from Program import Program
from utils import is_operation, is_conditional_branch, is_instruction, is_immediate, is_register, is_memory_address


class RulesHandler:
    NUMBER_OF_RULES = 19

    def possible_rules(self, program: Program, program_line: int, experimental_rules=False) -> list:
        """
        Return a list of all possible rules o apply
        :param program: program to analyze
        :param program_line: line of program to inspect
        :param experimental_rules: use experimental rules (marked with ¿?)
        :return: list of all possible rules
        """
        if program_line < program.length() - 1:
            rules = []
            actual_instruction = program.instruction(program_line)
            next_instruction = program.instruction(program_line + 1)
            # PUSH rules
            if actual_instruction == 'push':
                # rule 1
                if next_instruction == "pop":
                    if self._rule1_check(program_line, program):
                        rules.append(1)
                    # rule 2
                    elif self._rule2_check(program_line, program):
                        rules.append(2)
                # rule 13
                elif self._rule13_check(program_line, program):
                    rules.append(13)
            # MOV rules
            elif actual_instruction == 'mov':
                if next_instruction == 'push':
                    # rule 3
                    if self._rule3_check(program_line, program):
                        rules.append(3)
                    # rule 4
                    elif self._rule4_check(program_line, program):
                        rules.append(4)
                    # rule 19
                    elif experimental_rules and self._rule19_check(program_line, program):
                        rules.append(19)
                elif is_operation(next_instruction) and len(program.get_line(program_line + 1)) == 3:
                    # rule 7
                    if self._rule7_check(program_line, program):
                        rules.append(7)
                    # rule 8
                    elif self._rule8_check(program_line, program):
                        rules.append(8)
                    # rule 20
                    elif experimental_rules and self._rule20_check(program_line, program):
                        rules.append(20)
                elif next_instruction == 'call' or next_instruction == 'syscall':
                    # rule 10
                    if self._rule10_check(program_line, program):
                        rules.append(10)
                    # rule 11
                    elif self._rule11_check(program_line, program):
                        rules.append(11)
                    # rule 21
                    elif experimental_rules and self._rule21_check(program_line, program):
                        rules.append(21)
                elif next_instruction == 'jmp':
                    # rule 12
                    if self._rule12_check(program_line, program):
                        rules.append(12)
                    # rule 14
                    elif self._rule14_check(program_line, program):
                        rules.append(14)
                    # rule 22
                    elif experimental_rules and self._rule22_check(program_line, program):
                        rules.append(22)
            # POP rules
            elif actual_instruction == 'pop':
                if next_instruction == 'mov':
                    # rule 5
                    if self._rule5_check(program_line, program):
                        rules.append(5)
                    # rule 6
                    elif self._rule6_check(program_line, program):
                        rules.append(6)
                    # rule 23
                    elif experimental_rules and self._rule23_check(program_line, program):
                        rules.append(23)
                else:
                    # rule 9
                    if self._rule9_check(program_line, program):
                        rules.append(9)
                    # rule 24
                    elif self._rule24_check(program_line, program):
                        rules.append(24)
                    # rule 15
                    elif self._rule15_check(program_line, program):
                        rules.append(15)
                    # rule 25
                    elif experimental_rules and self._rule25_check(program_line, program):
                        rules.append(25)
            # CONDITIONAL BRANCH rules
            elif is_conditional_branch(actual_instruction):
                # rule 16
                if self._rule16_check(program_line, program):
                    rules.append(16)
                # rule 17
                elif self._rule17_check(program_line, program):
                    rules.append(17)
                # rule 18
                elif self._rule18_check(program_line, program):
                    rules.append(18)
            return rules
        else:
            return []

    def apply_all_rules_global(self, program: Program):
        """
        Apply all possible rules
        :return: None
        """
        program_line = 0
        while program_line < program.length():
            if self.apply_rules(program=program, program_line=program_line):
                program_line = 0
            program_line = program_line + 1

    def apply_rules(self, program: Program, program_line: int, experimental_rules=False) -> bool:
        """
        Apply all possible rules to a certain point
        :param program: program to modify
        :param program_line: line of program to apply the rule
        :param experimental_rules: use experimental rules (marked with ¿?)
        :return: True if at least one rule could be applied, False if else
        """
        rule_applied = False
        if program_line < program.length() - 1:
            actual_instruction = program.instruction(program_line)
            next_instruction = program.instruction(program_line + 1)
            if actual_instruction == 'push':
                if next_instruction == "pop":
                    rule_applied = True if self.rule1(program_line, program) else rule_applied
                    rule_applied = True if self.rule2(program_line, program) else rule_applied
                else:
                    rule_applied = True if self.rule13(program_line, program) else rule_applied
            elif program.instruction(program_line) == 'mov':
                if next_instruction == 'push':
                    rule_applied = True if self.rule3(program_line, program) else rule_applied
                    rule_applied = True if self.rule4(program_line, program) else rule_applied
                    if experimental_rules:
                        rule_applied = True if self.rule19(program_line, program) else rule_applied
                elif is_instruction(next_instruction) and len(program.instruction(program_line + 1)) == 3:
                    rule_applied = True if self.rule7(program_line, program) else rule_applied
                    rule_applied = True if self.rule8(program_line, program) else rule_applied
                    if experimental_rules:
                        rule_applied = True if self.rule20(program_line, program) else rule_applied
                elif next_instruction == "call" or next_instruction == 'syscall':
                    rule_applied = True if self.rule10(program_line, program) else rule_applied
                    rule_applied = True if self.rule11(program_line, program) else rule_applied
                    if experimental_rules:
                        rule_applied = True if self.rule21(program_line, program) else rule_applied
                elif next_instruction == 'jmp':
                    rule_applied = True if self.rule12(program_line, program) else rule_applied
                    rule_applied = True if self.rule14(program_line, program) else rule_applied
                    if experimental_rules:
                        rule_applied = True if self.rule22(program_line, program) else rule_applied
            elif program.instruction(program_line) == 'pop':
                if next_instruction == 'mov':
                    rule_applied = True if self.rule5(program_line, program) else rule_applied
                    rule_applied = True if self.rule6(program_line, program) else rule_applied
                    if experimental_rules:
                        rule_applied = True if self.rule23(program_line, program) else rule_applied
                else:
                    rule_applied = True if self.rule9(program_line, program) else rule_applied
                    rule_applied = True if self.rule24(program_line, program) else rule_applied
                    rule_applied = True if self.rule15(program_line, program) else rule_applied
                    if experimental_rules:
                        rule_applied = True if self.rule25(program_line, program) else rule_applied
            elif is_conditional_branch(program.instruction(program_line)):
                rule_applied = True if self.rule16(program_line, program) else rule_applied
                rule_applied = True if self.rule17(program_line, program) else rule_applied
                rule_applied = True if self.rule18(program_line, program) else rule_applied
        return rule_applied

    def _rule1_check(self, program_line: int, program: Program) -> bool:
        return (is_immediate(program, program.operand(program_line, 1))) and (
            is_register(program.operand(program_line + 1, 1)))

    def rule1(self, program_line: int, program: Program) -> bool:
        """
        PUSH Imm / POP Reg  <-->  MOV Reg,Imm
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule1_check(program_line, program):
            new_line = ["mov", program.operand(program_line + 1, 1), program.operand(program_line, 1)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule2_check(self, program_line: int, program: Program) -> bool:
        return (is_register(program.operand(program_line, 1))) and (
            is_register(program.operand(program_line + 1, 1))) and (
                       program.operand(program_line, 1) != program.operand(program_line + 1, 1))

    def rule2(self, program_line: int, program: Program) -> bool:
        """
        PUSH Reg / POP Reg2  <-->  MOV Reg2,Reg
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule2_check(program_line, program):
            new_line = ["mov", program.operand(program_line + 1, 1), program.operand(program_line, 1)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule3_check(self, program_line: int, program: Program) -> bool:
        return (is_memory_address(program.operand(program_line, 1))) and (
            is_immediate(program, program.operand(program_line, 2))) and (
                   is_immediate(program, program.operand(program_line + 1, 1))) and (
                       program.operand(program_line, 1) == program.operand(program_line + 1, 1))

    def rule3(self, program_line: int, program: Program) -> bool:
        """
        MOV Mem,Imm / PUSH Mem	<-->  PUSH Imm
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule3_check(program_line, program):
            new_line = ["push", program.operand(program_line, 2)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule4_check(self, program_line: int, program: Program) -> bool:
        return (is_memory_address(program.operand(program_line, 1))) and (
            is_register(program.operand(program_line, 2))) and (
                   is_memory_address(program.operand(program_line + 1, 1))) \
               and (program.operand(program_line, 1) == program.operand(program_line + 1, 1))

    def rule4(self, program_line: int, program: Program) -> bool:
        """
        MOV Mem,Reg / PUSH Mem   <-->  PUSH Reg
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule4_check(program_line, program):
            new_line = ["push", program.operand(program_line, 2)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule5_check(self, program_line: int, program: Program) -> bool:
        return (is_memory_address(program.operand(program_line, 1))) and (
            is_memory_address(program.operand(program_line + 1, 1))) and (
                   is_memory_address(program.operand(program_line + 1, 2))) and (
                       program.operand(program_line, 1) == program.operand(program_line + 1, 2)) and (
                       program.operand(program_line, 1) != program.operand(program_line + 1, 1))

    def rule5(self, program_line: int, program: Program) -> bool:
        """
        POP Mem2 / MOV Mem,Mem2	<-->  POP Mem
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule5_check(program_line, program):
            new_line = ["pop", program.operand(program_line + 1, 1)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule6_check(self, program_line: int, program: Program) -> bool:
        return (is_memory_address(program.operand(program_line, 1))) and (
            is_register(program.operand(program_line + 1, 1))) and (
                   is_memory_address(program.operand(program_line + 1, 2))) and (
                       program.operand(program_line, 1) == program.operand(program_line + 1, 2))

    def rule6(self, program_line: int, program: Program) -> bool:
        """
        POP Mem / MOV Reg,Mem  <-->  POP Reg
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule6_check(program_line, program):
            new_line = ["pop", program.operand(program_line + 1, 1)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule7_check(self, program_line: int, program: Program) -> bool:
        return (is_memory_address(program.operand(program_line, 1))) and (
            is_immediate(program, program.operand(program_line, 2))) and (
                   is_register(program.operand(program_line + 1, 1))) and (
                   is_memory_address(program.operand(program_line + 1, 2))) and (
                       program.operand(program_line, 1) == program.operand(program_line + 1, 2))

    def rule7(self, program_line: int, program: Program) -> bool:
        """
        MOV Mem,Imm / OP Reg,Mem  <-->  OP Reg,Imm
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule7_check(program_line, program):
            new_line = [program.instruction(program_line + 1), program.operand(program_line + 1, 1),
                        program.operand(program_line, 2)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule8_check(self, program_line: int, program: Program) -> bool:
        return (is_memory_address(program.operand(program_line, 1))) and (
            is_memory_address(program.operand(program_line, 2))) and (
                   is_register(program.operand(program_line + 1, 1))) and (
                   is_memory_address(program.operand(program_line + 1, 2))) and (
                       program.operand(program_line, 1) == program.operand(program_line + 1, 2))

    def rule8(self, program_line: int, program: Program) -> bool:
        """
        MOV Mem2,Mem / OP Reg,Mem2  <-->  OP Reg,Mem
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule8_check(program_line, program):
            new_line = [program.instruction(program_line + 1), program.operand(program_line + 1, 1),
                        program.operand(program_line, 2)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule9_check(self, program_line: int, program: Program) -> bool:
        return program.instruction(program_line + 1) == "push" and (
            is_memory_address(program.operand(program_line, 1))) and (
                   is_memory_address(program.operand(program_line + 1, 1))) and (
                       program.operand(program_line, 1) == program.operand(program_line + 1, 1))

    def rule9(self, program_line: int, program: Program) -> bool:
        """
        POP Mem / PUSH Mem  <-->  NOP
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule9_check(program_line, program):
            program.delete(program_line)
            program.delete(program_line)
            return True
        else:
            return False

    def _rule10_check(self, program_line: int, program: Program) -> bool:
        return (is_memory_address(program.operand(program_line, 1))) and (
            is_register(program.operand(program_line, 2))) and (
                   is_memory_address(program.operand(program_line + 1, 1))) and (
                       program.operand(program_line, 1) == program.operand(program_line + 1, 1))

    def rule10(self, program_line: int, program: Program) -> bool:
        """
        MOV Mem,Reg / CALL Mem  <-->  CALL Reg
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule10_check(program_line, program):
            call_type = program.instruction(program_line + 1)
            new_line = [call_type, program.operand(program_line, 2)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule11_check(self, program_line: int, program: Program) -> bool:
        return (is_memory_address(program.operand(program_line, 1))) and (
            is_memory_address(program.operand(program_line, 2))) and (
                   is_memory_address(program.operand(program_line + 1, 1))) and (
                       program.operand(program_line, 1) == program.operand(program_line + 1, 1))

    def rule11(self, program_line: int, program: Program) -> bool:
        """
        MOV Mem2,Mem / CALL Mem2  <-->  CALL Mem
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule11_check(program_line, program):
            call_type = program.instruction(program_line + 1)
            new_line = [call_type, program.operand(program_line, 2)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule12_check(self, program_line: int, program: Program) -> bool:
        return program.instruction(program_line + 1) == "jmp" and (
            is_memory_address(program.operand(program_line, 1))) and (
                   is_register(program.operand(program_line, 2))) and (
                   is_memory_address(program.operand(program_line + 1, 1))) and (
                       program.operand(program_line, 1) == program.operand(program_line + 1, 1))

    def rule12(self, program_line: int, program: Program) -> bool:
        """
        MOV Mem,Reg / JMP Mem  <-->  JMP Reg
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule12_check(program_line, program):
            new_line = ["jmp", program.operand(program_line, 2)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule13_check(self, program_line: int, program: Program) -> bool:
        return program.instruction(program_line + 1) == "ret" and is_register(program.operand(program_line, 1))

    def rule13(self, program_line: int, program: Program) -> bool:
        """
        PUSH Reg / RET  <-->  JMP Reg
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule13_check(program_line, program):
            new_line = ["jmp", program.operand(program_line, 1)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule14_check(self, program_line: int, program: Program) -> bool:
        return program.instruction(program_line + 1) == "jmp" and (
            is_memory_address(program.operand(program_line, 1))) and (
                   is_memory_address(program.operand(program_line, 2))) and (
                   is_memory_address(program.operand(program_line + 1, 1))) and (
                       program.operand(program_line, 1) == program.operand(program_line + 1, 1))

    def rule14(self, program_line: int, program: Program) -> bool:
        """
        MOV Mem2,Mem / JMP Mem2  <-->  JMP Mem
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule14_check(program_line, program):
            new_line = ["jmp", program.operand(program_line, 2)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule15_check(self, program_line: int, program: Program) -> bool:
        return program.instruction(program_line + 1) == "jmp" and (
            is_memory_address(program.operand(program_line, 1))) and (
                   is_memory_address(program.operand(program_line + 1, 1))) and (
                       program.operand(program_line, 1) == program.operand(program_line + 1, 1))

    def rule15(self, program_line: int, program: Program) -> bool:
        """
        POP Mem / JMP Mem  <-->  RET
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule15_check(program_line, program):
            new_line = ["ret"]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule16_check(self, program_line: int, program: Program) -> bool:
        return (program.operand(program_line, 1) == program.operand(program_line + 1, 1)) and (
                ((program.instruction(program_line) == "je") and (program.instruction(program_line + 1) == "jne")) or
                ((program.instruction(program_line) == "jne") and (program.instruction(program_line + 1) == "je")))

    def rule16(self, program_line: int, program: Program) -> bool:
        """
        JE Reg / JNE Reg <---> JMP Reg
        or
        JNE Reg / JE Reg <---> JMP Reg
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule16_check(program_line, program):
            new_line = ["jmp", program.operand(program_line, 1)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule17_check(self, program_line: int, program: Program) -> bool:
        return (program.operand(program_line, 1) == program.operand(program_line + 1, 1)) and (
                ((program.instruction(program_line) == "jl") and (program.instruction(program_line + 1) == "jge")) or
                ((program.instruction(program_line) == "jge") and (program.instruction(program_line + 1) == "jl")))

    def rule17(self, program_line: int, program: Program) -> bool:
        """
        JL Reg / JGE Reg <---> JMP Reg
        or
        JGE Reg / JL Reg <---> JMP Reg
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule17_check(program_line, program):
            new_line = ["jmp", program.operand(program_line, 1)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule18_check(self, program_line: int, program: Program) -> bool:
        return (program.operand(program_line, 1) == program.operand(program_line + 1, 1)) and (
                ((program.instruction(program_line) == "jg") and (program.instruction(program_line + 1) == "jle")) or
                ((program.instruction(program_line) == "jle") and (program.instruction(program_line + 1) == "jg")))

    def rule18(self, program_line: int, program: Program) -> bool:
        """
        JG Reg / JLE Reg <---> JMP Reg
        or
        JLE Reg / JG Reg <---> JMP Reg
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule18_check(program_line, program):
            new_line = ["jmp", program.operand(program_line, 1)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule19_check(self, program_line: int, program: Program) -> bool:
        return (is_register(program.operand(program_line, 1))) and (
            is_register(program.operand(program_line, 2))) and \
               (program.operand(program_line, 1) == program.operand(program_line + 1, 1))

    def rule19(self, program_line: int, program: Program) -> bool:
        """
        MOV Reg,Reg2 / PUSH Reg  <---> PUSH Reg2
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule19_check(program_line, program):
            new_line = ["push", program.operand(program_line, 2)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule20_check(self, program_line: int, program: Program) -> bool:
        return (is_register(program.operand(program_line, 1))) and (
            is_immediate(program, program.operand(program_line, 2))) and (
                   is_register(program.operand(program_line + 1, 1))) and \
               (program.operand(program_line, 1) == program.operand(program_line + 1, 2))

    def rule20(self, program_line: int, program: Program) -> bool:
        """
        MOV Reg,Imm / OP Reg2,Reg <--> OP Reg2,Imm
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule20_check(program_line, program):
            new_line = [program.instruction(program_line + 1), program.operand(program_line + 1, 1),
                        program.operand(program_line, 2)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule21_check(self, program_line: int, program: Program) -> bool:
        return (is_register(program.operand(program_line, 1))) and (
            is_register(program.operand(program_line, 2))) and \
               (program.operand(program_line, 1) == program.operand(program_line + 1, 1))

    def rule21(self, program_line: int, program: Program) -> bool:
        """
        MOV Reg2,Reg / CALL Reg2 <--> CALL Reg
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule21_check(program_line, program):
            call_type = program.instruction(program_line + 1)
            new_line = [call_type, program.operand(program_line, 2)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule22_check(self, program_line: int, program: Program) -> bool:
        return program.instruction(program_line + 1) == "jmp" and (
            is_register(program.operand(program_line, 1))) and (
                   is_register(program.operand(program_line, 2))) and (
                       program.operand(program_line, 1) == program.operand(program_line + 1, 1))

    def rule22(self, program_line: int, program: Program) -> bool:
        """
        MOV Reg2,Reg / JMP Reg2  <-->  JMP Reg
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule22_check(program_line, program):
            new_line = ["jmp", program.operand(program_line, 2)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule23_check(self, program_line: int, program: Program) -> bool:
        return (is_register(program.operand(program_line, 1))) and (
            is_register(program.operand(program_line + 1, 1))) and (
                       program.operand(program_line, 1) == program.operand(program_line + 1, 2))

    def rule23(self, program_line: int, program: Program) -> bool:
        """
        POP Reg / MOV Reg2,Reg  <-->  POP Reg2
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule23_check(program_line, program):
            new_line = ["pop", program.operand(program_line + 1, 1)]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False

    def _rule24_check(self, program_line: int, program: Program) -> bool:
        return program.instruction(program_line + 1) == "push" and (
            is_register(program.operand(program_line, 1))) and (
                       program.operand(program_line, 1) == program.operand(program_line + 1, 1))

    def rule24(self, program_line: int, program: Program) -> bool:
        """
        POP Reg / PUSH Reg <--> NOP
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule24_check(program_line, program):
            program.delete(program_line)
            program.delete(program_line)
            return True
        else:
            return False

    def _rule25_check(self, program_line: int, program: Program) -> bool:
        return program.instruction(program_line + 1) == "jmp" and (
            is_register(program.operand(program_line, 1))) and (
                   is_register(program.operand(program_line + 1, 1))) and (
                       program.operand(program_line, 1) == program.operand(program_line + 1, 1))

    def rule25(self, program_line: int, program: Program) -> bool:
        """
        POP Reg / JMP Reg <--> RET
        :param program: program to modify
        :param program_line: line where the rule should be applied
        :return: True if rule can be applied, False if else
        """
        if self._rule25_check(program_line, program):
            new_line = ["ret"]
            program.delete(program_line)
            program.delete(program_line)
            program.insert_to_instructions(program_line, new_line)
            return True
        else:
            return False
