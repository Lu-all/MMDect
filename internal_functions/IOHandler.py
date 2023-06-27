import os
import sys
from os.path import exists
from pathlib import Path
from typing import List

from internal_functions.colored import print_error
from internal_functions.program import Program
from internal_functions.utils import is_instruction, is_register, is_immediate, is_conditional_branch, is_branch, \
    is_memory_address


def get_prolog_signatures(signatures_path: str) -> tuple[List[str], List[List[str]]]:
    """
    Read prolog signatures from a given path
    :param signatures_path: Parent directory where the signatures are located
    :return: Names and content of signatures
    """
    if not exists(signatures_path):
        print("Signature path not valid. Check -s or --signatures argument and try again.")
        sys.exit(3)
    signatures_array = []
    names_array = []
    for file in Path(signatures_path).glob('**/*.prologsign'):
        signature = file.read_text().split('\n')
        names_array.append(signature[0])
        signatures_array.append(signature[1:])
    return names_array, signatures_array


def get_regex_signatures(signatures_path: str) -> tuple[List[str], List[List[str]]]:
    """
    Read regex signatures from a given path
    :param signatures_path: Parent directory where the signatures are located
    :return: Names and content of signatures
    """
    if not exists(signatures_path):
        print("Signature path not valid. Check -s or --signatures argument and try again.")
        sys.exit(3)
    signatures_array = []
    names_array = []
    for file in Path(signatures_path).glob('**/*.txt'):
        signature = file.read_text().split('\n')
        names_array.append(signature[0])
        signatures_array.append(signature[1:])
    return names_array, signatures_array


def write_program(program: Program, name: str, att_syntax: bool, use_tag_replacement=False) -> None:
    """
    Write current instructions in variable program to file
    :param program: program to export to file
    :param name: name of the new file
    :param att_syntax: True to write output program in ATT syntax
    :param use_tag_replacement: True to use experimental replacement of tags
    :return: None
    """
    if use_tag_replacement:
        program.line_to_tags()
    f = open(name, 'w')
    if att_syntax:
        for line in program.header:
            f.write(line.replace(';', '#').replace('_start', 'main'))
        for i in range(0, program.length()):
            if len(program.get_line(i)) >= 2:  # type: ignore
                first_operand_prev = ''
                first_operand = str(program.operand(i, 1))
                if is_register(program.operand(i, 1)):  # type: ignore
                    first_operand_prev = "%"
                elif is_immediate(program, program.operand(i, 1)):  # type: ignore
                    first_operand_prev = "$"
                elif is_memory_address(program.operand(i, 1)):
                    address = first_operand.split('[')[1].split(']')[0]
                    if is_register(address):
                        first_operand = "(" + "%" + address + ")"
                    else:
                        first_operand = "(" + "$" + address + ")"
                if len(program.get_line(i)) == 3:  # type: ignore
                    second_operand_prev = ''
                    second_operand = str(program.operand(i, 2))
                    if is_register(program.operand(i, 2)):  # type: ignore
                        second_operand_prev = "%"
                    elif is_immediate(program, program.operand(i, 2)):  # type: ignore
                        second_operand_prev = "$"
                    elif is_memory_address(program.operand(i, 1)):
                        address = second_operand.split('[')[1].split(']')[0]
                        if is_register(address):
                            second_operand = "(" + "%" + address + ")"
                        else:
                            second_operand = "(" + "$" + address + ")"
                    s = str(program.instruction(
                        i)) + ' ' + second_operand_prev + second_operand + ', ' + first_operand_prev + \
                        first_operand
                else:
                    s = str(program.instruction(i)) + ' ' + first_operand_prev + first_operand
            else:
                s = str(program.instruction(i))
            f.write(s)
            f.write('\n')
    else:
        for line in program.header:
            f.write(line)
        for i in range(0, program.length()):
            if len(program.get_line(i)) == 2:  # type: ignore
                s = str(program.instruction(i)) + ' ' + str(program.operand(i, 1))
            elif len(program.get_line(i)) == 3:  # type: ignore
                s = str(program.instruction(i)) + ' ' + str(program.operand(i, 1)) + ', ' + str(program.operand(i, 2))
            else:
                s = str(program.instruction(i))
            f.write(s)
            f.write('\n')
    f.flush()
    f.close()


def read_programs(path: str, multiple_input: bool, entry_point: str, silent: bool) -> List[Program]:
    """


    :param path: Path where files are located
    :param multiple_input: True if multiple files are given as an input
    :param entry_point: Name of custom entry point (_start by default)
    :param silent: True to hide output
    :return:  List of Programs transformed to intermediate language
    """
    programs = []
    if multiple_input:
        for filename in os.listdir(path):
            if filename.endswith(".txt"):
                programs.append(read_program(path + "/" + filename, entry_point, silent))
    else:
        programs.append(read_program(path, entry_point, silent))
    if len(programs) == 0:
        print_error("Error: the given directory is empty or it has been an unexpected error reading files.\n"
                    "Warning: files must have .txt extension", silent)
        sys.exit()
    return programs


def read_program(name: str, entry_point: str, silent: bool, tag_replace_to_numbers=False) -> Program:
    """
    Read file specified and parse it to program
    :param name: name of file where program is written
    :param entry_point: Name of custom entry point (_start by default)
    :param silent: True to hide output
    :param tag_replace_to_numbers: True to use experimental tag substitution
    :return: program
    """
    try:
        program = Program()
        with open(name, 'r') as file:
            program_file = file.read().split("\n")
            header_skipped = False
            num_line = -1
            comment_symbol = ';'
            for line in program_file:
                if header_skipped:
                    if line.find('#'):
                        comment_symbol = '#'
                        has_comment = True
                    elif line.find(';'):
                        comment_symbol = ';'
                        has_comment = True
                    if not has_comment:
                        continue
                    elif has_comment:
                        line = line.split(comment_symbol)[0].strip()
                    if line.isspace() or len(line) == 0:
                        continue
                    num_line = num_line + 1
                    line = line.replace(",", " ")
                    line = line.replace("\t", " ")
                    if line.find(";") != -1:
                        line = line.split(";")[0]
                    if line.find(":") != -1:
                        tag = line.split(":")[0]
                        if tag not in program.to_replace:
                            program.to_replace.append(tag)
                            program.tags.append(tag)
                            program.num_tags.append(num_line)
                            if tag_replace_to_numbers:
                                program.add_instruction(int(len(program.to_replace)))
                            else:
                                program.add_instruction([line])
                        continue
                    line = line.split()
                    if len(line) > 0 and is_instruction(line[0]):
                        if (is_conditional_branch(line[0]) or is_branch(line[0])) and len(line) > 2:
                            line = [line[0], line[len(line) - 1]]
                        program.add_instruction(line)
                elif line.find(entry_point) == 0:
                    program.add_to_header(line + "\n")
                    header_skipped = True
                else:
                    program.add_to_header(line + "\n")
            if tag_replace_to_numbers:
                program.tag_replacement()
        file.close()
        return program
    except IOError:
        print_error("Error: file could not be read", silent=silent)
        sys.exit()
