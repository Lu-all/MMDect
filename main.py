import getopt
import sys
from os.path import exists

from IOHandler import read_program, write_program
from compressor import compress_program


def merubacc_help():
    print("Code based in MetaSign metamorphic rules in compression mode. "
          "Input file must have a '_start' tag at the beginning of code.\n"
          "Currently, .data section is not supported.\nUsage: "
          "-h or --help to display this guide\n"
          "-i or --intel_output to write the output file in intel syntax (ATT syntax is selected by default)\n"
          "-t or --tag_replacement to use an experimental substitution of tags to line number while applying rules\n"
          "-f or --file to specify input file. If not specified, it will use examples/passwddump.txt as input\n"
          "-o or --output to specify name of output file. If not specified, it will be <file>-compressed.<extension>")


example_path = "examples/passwddump.txt"
att_syntax = True
tag_replacement = False
default_output = True
name: str = example_path
name_output = example_path + "-compressed.txt"
command_args = sys.argv[1:]
options: list[tuple[str, str]] = [('-f', example_path)]
arguments: list[str] = [example_path]
try:
    options, arguments = getopt.getopt(command_args, "hitf:o:",
                                       ["help", "intel_output", "tag_replacement", "file=", "output="])
except getopt.GetoptError as error:
    print(error)
    exit(2)
for option, argument in options:
    if option in ['-h', '--help']:
        merubacc_help()
    elif option in ['-i', '--intel_output']:
        att_syntax = False
    elif option in ['-t', '--tag_replacement']:
        tag_replacement = True
    elif option in ['-f', '--file']:
        name = str(argument)
    elif option in ['-o', '--output']:
        name_output = str(argument)
        default_output = False
if exists(name):
    name_without_extension = name.split('.')[0]
    extension = name.split('.')[-1]
    program = read_program(name=name, tag_replace_to_numbers=tag_replacement)
    program = compress_program(program)
    if default_output:
        name_output = name_without_extension + "-compressed." + extension
    write_program(name=name_output, att_syntax=att_syntax,
                  program=program, use_tag_replacement=tag_replacement)
    exit(0)
else:
    print("Input file does not exist or is not specified. Check -f or --file argument and try again.")
    exit(3)
