import sys
from os.path import exists
from pathlib import Path

from internal_functions.program import Program
from internal_functions.utils import is_instruction, is_register, is_immediate, is_conditional_branch, is_branch


def get_signatures(signatures_path: str, python_exec: int) -> list[list[str], list[list[str]]]:
    """
    Reads signatures from a given path
    :param signatures_path: Parent directory where the signatures are located
    :param python_exec: Indicates type of signatures to read
    :return: Names and content of signatures
    """
    if not exists(signatures_path):
        print("Signature path not valid. Check -s or --signatures argument and try again.")
        sys.exit(3)
    signatures_array = []
    names_array = []
    end_files = "txt" if python_exec > 1 else "prologsign"
    for file in Path(signatures_path).glob('**/*.' + end_files):
        signature = file.read_text().split('\n')
        names_array.append(signature[0])
        signatures_array.append(signature[1:])
    return [names_array, signatures_array]


def write_program(program: Program, name: str, att_syntax: bool, use_tag_replacement=False):
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
    for line in program.header:
        f.write(line)
    if att_syntax:
        for i in range(0, program.length()):
            if len(program.get_line(i)) >= 2:
                first_operand = ''
                if is_register(program.operand(i, 1)):
                    first_operand = "%"
                elif is_immediate(program, program.operand(i, 1)):
                    first_operand = "$"
                if len(program.get_line(i)) == 3:
                    second_operand = ''
                    if is_register(program.operand(i, 2)):
                        second_operand = "%"
                    elif is_immediate(program, program.operand(i, 2)):
                        second_operand = "$"
                    s = str(program.instruction(i)) + ' ' + second_operand + str(
                        program.operand(i, 2)) + ', ' + first_operand + \
                        str(program.operand(i, 1))
                else:
                    s = str(program.instruction(i)) + ' ' + first_operand + str(program.operand(i, 1))
            else:
                s = str(program.instruction(i))
            f.write(s)
            f.write('\n')
    else:
        for i in range(0, program.length()):
            if len(program.get_line(i)) == 2:
                s = str(program.instruction(i)) + ' ' + str(program.operand(i, 1))
            elif len(program.get_line(i)) == 3:
                s = str(program.instruction(i)) + ' ' + str(program.operand(i, 1)) + ', ' + str(program.operand(i, 2))
            else:
                s = str(program.instruction(i))
            f.write(s)
            f.write('\n')
    f.flush()
    f.close()


def read_program(name: str, tag_replace_to_numbers=False) -> Program:
    """
    Read file specified and parse it to program
    :param name: name of file where program is written
    :param tag_replace_to_numbers: True to use experimental tag substitution
    :return: program
    """
    try:
        program = Program()
        with open(name, 'r') as file:
            program_file = file.read().split("\n")
            header_skipped = False
            num_line = -1
            for line in program_file:
                if header_skipped:
                    has_comment = line.find('#')
                    if has_comment == 0:
                        continue
                    elif has_comment > 0:
                        line = line.split("#")[0].strip()
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
                elif line.find("_start") == 0:
                    program.add_to_header(line + "\n")
                    header_skipped = True
                else:
                    program.add_to_header(line + "\n")
            if tag_replace_to_numbers:
                program.tag_replacement()
        file.close()
        return program
    except IOError:
        print("Error: file could not be read")
        sys.exit()
