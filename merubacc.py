import getopt
import sys
from os.path import exists

from internal_functions.IOHandler import read_program, write_program, get_signatures
from rules.modeHandler import compress_program, compress_and_compare_program, compare_program


def merubacc_help() -> None:
    """
    Display help
    :return: None
    """
    print("Code based in MetaSign metamorphic rules in compression mode."
          "Input file must have a '_start' tag at the beginning of instructions.\n"
          "Usage: "
          "-h or --help to display this guide\n"

          "-m or --mode to specify mode between: compress-only (only execute compression module), compare-only"
          " (only execute comparation module or both (execute both modules). Both is selected by default.\n"

          "-a or --att_syntax to write the output file in ATT syntax (Intel syntax is selected by default)\n"

          "-p or --python to execute compression, comparation or both in python instead of prolog\n"

          "-f or --file to specify input file. If not specified, it will use examples/passwddump.txt as input\n"

          "-o or --output to specify name of output file. If not specified, it will be <file>-compressed.<extension>\n"

          "-s or --signatures to specify path of signatures parent directory, which also enables compare step."
          "Rules for prolog calculation should have '.prologsign' extension, "
          "while rules extension for comparation in python must be '.txt'. Python rules can be in regex format.\n")


example_path = "examples/passwddump.txt"
python_exec = 0
att_syntax = False
tag_replacement = False
default_output = True
mode = "both"
positives = []
name: str = example_path
name_output = example_path + "-compressed.txt"
signatures_path = "example_signatures/"
command_args = sys.argv[1:]
options: list[tuple[str, str]] = [('-f', example_path)]
arguments: list[str] = [example_path]
try:
    options, arguments = getopt.getopt(command_args, "hatp:f:o:s:m:",
                                       ["help", "att_syntax", "tag_replacement", "python=", "file=", "output=",
                                        "signatures=", "mode="])
except getopt.GetoptError as error:
    print(error)
    sys.exit(2)
for option, argument in options:
    if option in ['-h', '--help']:
        merubacc_help()
    elif option in ['-p', '--python']:
        python_exec = str(argument)
    elif option in ['-m', '--mode']:
        mode = str(argument)
    elif option in ['-a', '--att_syntax']:
        att_syntax = True
    elif option in ['-t', '--tag_replacement']:
        print("Currently, this option is in development")
        tag_replacement = False
    elif option in ['-f', '--file']:
        name = str(argument)
    elif option in ['-o', '--output']:
        name_output = str(argument)
        default_output = False
    elif option in ['-s', '--signatures']:
        signatures_path = str(argument)
if exists(name):
    name_without_extension = name.split('.')[0]
    extension = name.split('.')[-1]
    program = read_program(name=name)
    switch = {
        "both": 3,
        "compare": 2,
        "compress": 1,
        "none": 0
    }
    python_exec = switch.get(python_exec, 0)
    if mode == "compare-only":
        names, signatures = get_signatures(signatures_path, python_exec=python_exec)
        positives = compare_program(program=program, signatures=signatures, names=names, python_exec=python_exec)
    else:
        if mode == "compress-only":
            programs = compress_program(program=program, python_exec=python_exec)
        else:
            names, signatures = get_signatures(signatures_path, python_exec=python_exec)
            programs, positives = compress_and_compare_program(program=program, signatures=signatures, names=names,
                                                               python_exec=python_exec)
        num_program = 0
        for program in programs:
            if default_output:
                name_output = name_without_extension + "-compressed." + extension
            write_program(name=name_output + "-" + str(num_program), att_syntax=att_syntax,
                          program=program)
            num_program = num_program + 1
    if len(positives) > 0:
        print(str(positives))
    sys.exit(0)
else:
    print("Input file does not exist or is not specified. Check -f or --file argument and try again.")
    sys.exit(3)
