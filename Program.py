from __future__ import annotations

from typing import Union


class Program:
    def __init__(self):
        self.header = []
        self.instructions = []
        self.to_replace = []
        self.tags = []
        self.num_tags = []

    def length(self) -> int:
        """
        Return total length of program (without header)
        :return: length (integer)
        """
        return len(self.instructions)

    def add_instruction(self, instruction: Union[list, int]) -> None:
        """
        Add instruction to program
        :param instruction: instruction to add
        :return: None
        """
        self.instructions.append(instruction)

    def insert_to_instructions(self, line: int, instruction: list):
        """
        Add instruction to program
        :param instruction: instruction to add
        :param line: line where to add instruction
        :return: None
        """
        self.instructions.insert(line, instruction)

    def add_to_header(self, header_line: str) -> None:
        """
        Add line to program header
        :param header_line: line to add
        :return: None
        """
        self.header.append(header_line)

    def get_line(self, number_of_line: int) -> Union[list, int, None]:
        """
        Return instruction in line specified
        :param number_of_line: number of instruction to return
        :return: instruction asked or None if not found
        """
        try:
            return self.instructions[number_of_line]
        except IndexError:
            return None

    def instruction(self, program_line: int) -> str | None:
        """
        Return instruction of program line
        :param program_line: number of line (integer) to extract the instruction
        :return: instruction or None if not found
        """
        try:
            return self.instructions[program_line][0]
        except IndexError:
            return None

    def operand(self, program_line: int, num_of_operand: int) -> str | None:
        """
        Return operand of program line
        :param program_line: number of line (integer) to extract the operand
        :param num_of_operand: number of operand (integer) to extract
        :return: operand or None if not found
        """
        try:
            return self.instructions[program_line][num_of_operand]
        except IndexError:
            return None

    def replace_operand(self, line: int, number_of_operand: int, new_operand) -> None:
        """
        Replace operand of program line
        :param line: number of instruction line (integer) where to replace operand
        :param number_of_operand: number of operand (integer) to replace
        :param new_operand: new operand
        :return: operand or None if not found
        """
        try:
            self.instructions[line][number_of_operand] = new_operand
        except IndexError:
            pass

    def tag_replacement(self):
        """
        Replace tag with number of line. Experimental use.
        :return: None
        """
        length = self.length()
        line = 0
        while line < length:
            if self.get_line(line).__class__ == int:
                tag = self.to_replace[self.get_line(line) - 1]
                self.to_replace[self.get_line(line) - 1] = [tag, line + 1]
                self.delete(line)
                length = length - 1
            line = line + 1
        for line in range(0, self.length()):
            if (len(self.get_line(line)) >= 2 and self.operand(line, 1) in self.tags) or (
                    len(self.get_line(line)) == 3 and self.operand(line, 2) in self.tags):
                for n in range(0, len(self.to_replace)):
                    self.instructions[line] = [self.num_tags[n] if i == self.tags[n] else i for i in
                                               self.instructions[line]]

    def _return_line_of_tag(self, tag_to_replace: list) -> int:
        return tag_to_replace[1]

    def line_to_tags(self):
        """
        Replace line jumps with original tags (tag_replacement reverse). Experimental use.
        :return: None
        """
        self.to_replace.sort(key=self._return_line_of_tag, reverse=True)
        for n in range(0, len(self.to_replace)):
            tag = self.to_replace[n][0] + ":"
            self.insert_to_instructions(self.to_replace[n][1] - 1, [tag])
        for line in range(self.length() - 1, 0, -1):
            if self.operand(line, 1) in self.num_tags:
                replaced = False
                num_tag = 0
                while not replaced and num_tag < len(self.to_replace):
                    if self.operand(line, 1) == self.to_replace[num_tag][1]:
                        self.replace_operand(line, 1, self.to_replace[num_tag][0])
                        replaced = True
                    num_tag = num_tag + 1

    def delete(self, program_line):
        self.instructions.pop(program_line)
