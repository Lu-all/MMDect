import getopt
import sys
from os.path import exists

from internal_functions.IOHandler import read_program, write_program
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

          "-r or --regex to enable both Regex and Prolog comparation. This option overwrites -p / --python argument.\n"

          "-f or --file to specify input file. If not specified, it will use examples/passwddump.txt as input\n"

          "-o or --output to specify name of output file. If not specified, it will be <file>-compressed.<extension>\n"

          "-s or --signatures to specify path of signatures parent directory, which also enables compare step."
          "Rules for prolog calculation should have '.prologsign' extension, "
          "while rules extension for comparation in python must be '.txt'. Python rules can be in regex format.\n")


example_path = "examples/passwddump.txt"
python_exec = "none"
att_syntax = False
tag_replacement = False
default_output = True
regex = False
mode = "both"
positives = []
name: str = example_path
name_output = example_path + "-compressed.txt"
signatures_path = "example_signatures/"
command_args = sys.argv[1:]
options: list[tuple[str, str]] = [('-f', example_path)]
arguments: list[str] = [example_path]
try:
    options, arguments = getopt.getopt(command_args, "hatrp:f:o:s:m:",
                                       ["help", "att_syntax", "tag_replacement", "python=", "file=", "output=",
                                        "signatures=", "mode=", "regex"])
except getopt.GetoptError as error:
    print(error)
    sys.exit(2)
for option, argument in options:
    if option in ['-h', '--help']:
        merubacc_help()
    elif option in ['-p', '--python']:
        if str(argument) not in ["none", "both", "compress", "compare"]:
            print("Not valid -p or --python argument, defaulting to 'none'")
        else:
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
    elif option in ['-r', '--regex']:
        regex = True
if exists(name):
    program = read_program(name=name)
    if default_output:
        name_output_array = name.split('.')
        extension = name_output_array[-1]
        name_without_extension = name_output_array[0]
        name_output_array = name_output_array[1::len(name_output_array)]
        for cut in name_output_array:
            name_without_extension = name_without_extension + '.' + cut
        name_without_extension = name_without_extension + "-compressed"
    else:
        name_output_array = name_output.split('.')
        extension = name_output_array[-1]
        name_without_extension = name_output_array[0]
        name_output_array = name_output_array[1::len(name_output_array)]
        for cut in name_output_array:
            name_without_extension = name_without_extension + '.' + cut
    if mode == "compare-only":
        positives = compare_program(program=program, path=signatures_path, python_exec=python_exec,
                                    regex_signatures=regex)
    else:
        if mode == "compress-only":
            programs = compress_program(program=program, python_exec=python_exec)
        else:
            programs, positives = compress_and_compare_program(program=program, path=signatures_path,
                                                               python_exec=python_exec, regex_signatures=regex)
        num_program = 0
        for program in programs:
            name = name_without_extension + "-" + str(num_program) + "." + extension
            write_program(name=name, att_syntax=att_syntax,
                          program=program)
            num_program = num_program + 1
    if len(positives) > 0:
        print(str(positives))
    sys.exit(0)
else:
    print("Input file does not exist or is not specified. Check -f or --file argument and try again.")
    sys.exit(3)
