import getopt
import sys
from os.path import exists

from internal_functions.IOHandler import read_program, write_program
from internal_functions.colored import print_error, print_pass, print_warn, print_banner, prints
from rules.modeHandler import compress_program, compress_and_compare_program, compare_program


def merubacc_help() -> None:
    """
    Display help
    :return: None
    """
    prints("Code based in MetaSign metamorphic rules in compression mode."
           "Input file must have a '_start' tag at the beginning of instructions.\n"
           "Usage: "
           "-h or --help to display this guide\n"

           "-v or --verbose to show output (true by default)\n"

           "-m or --mode to specify mode between: compress-only (only execute compression module), compare-only"
           " (only execute comparation module or both (execute both modules). Both is selected by default.\n"

           "-a or --att_syntax to write the output file in ATT syntax (Intel syntax is selected by default)\n"

           "-p or --python to execute compression, comparation or both in python instead of prolog\n"

           "-c or --compare-both to compare both Regex and Prolog signatures (overwrites -p python in comparation).\n"

           "-f or --file to specify input file. If not specified, it will use examples/passwddump.txt as input\n"

           "-o or --output to specify name of output file. If not specified, it will be <file>-compressed.<extension>\n"

           "-O or --positives_output to write positives to a file. If not specified,"
           "positives will be printed in standard output (even in silent mode)"

           "-s or --signatures to specify path of signatures parent directory, which also enables compare step."
           "Rules for prolog calculation should have '.prologsign' extension, "
           "while rules extension for comparation in python must be '.txt'. Python rules can be in regex format.\n",
           silent)


silent = False
example_path = "examples/passwddump.txt"
positives_file = False
command_args = sys.argv[1:]
options: list[tuple[str, str]] = [('-f', example_path)]
arguments: list[str] = [example_path]
try:
    options, arguments = getopt.getopt(command_args, "hatcvp:f:o:O:s:m:",
                                       ["help", "att_syntax", "tag_replacement", "verbose", "compare-both", "python=",
                                        "file=",
                                        "output=", "positives_output=", "signatures=", "mode="])
except getopt.GetoptError as error:
    print(error)
    sys.exit(2)
for option, argument in options:
    if option in ['-v', '--verbose']:
        if str(argument).casefold() == 'true':
            prints("\t[+] Verbose mode enabled", silent)
            silent = False
        elif str(argument).casefold() == 'false':
            prints("\t[+] Silent mode enabled", silent)
            silent = True
        else:
            print_error("\t[!] Not valid -v or --verbose argument, defaulting to 'true'", silent)

print_banner("""
  __  __ ___ ___ _   _ ___   _   ___ ___ 
 |  \/  | __| _ \ | | | _ ) /_\ / __/ __|
 | |\/| | _||   / |_| | _ \/ _ \ (_| (__ 
 |_|  |_|___|_|_\\___/|___/_/ \_\___\___|
""", silent)
prints("[+] Importing defaults", silent)
python_exec = "none"
att_syntax = False
tag_replacement = False
default_output = True
both_signatures = False
mode = "both"
positives = []
name: str = example_path
name_output = example_path + "-compressed.txt"
signatures_path = "example_signatures/"
prints("[-] Reading arguments", silent)
for option, argument in options:
    if option in ['-p', '--python']:
        if str(argument) not in ["none", "both", "compress", "compare"]:
            print_error("\t[!] Not valid -p or --python argument, defaulting to 'none'", silent)
        else:
            prints("\t[+] Python mode = " + str(argument), silent)
            python_exec = str(argument)
    elif option in ['-m', '--mode']:
        prints("\t[+] Mode = " + str(argument), silent)
        mode = str(argument)
    elif option in ['-a', '--att_syntax']:
        prints("\t[+] Syntax = ATT", silent)
        att_syntax = True
    elif option in ['-t', '--tag_replacement']:
        print_warn("\t[+] Tag replacement disabled", silent)
        print_error("\t\t[!] Currently, this option is in development", silent)
        tag_replacement = False
    elif option in ['-o', '--output']:
        print_warn("\t[+] Output file = " + str(argument), silent)
        name_output = str(argument)
        default_output = False
    elif option in ['-O', '--positives_output']:
        print_warn("\t[+] Positives output file = " + str(argument), silent)
        positives_file = str(argument)
    elif option in ['-s', '--signatures']:
        print_warn("\t[+] Signatures path = " + str(argument), silent)
        signatures_path = str(argument)
    elif option in ['-c', '--compare-both']:
        prints("\t[+] Both types of signatures will be used.", silent)
        both_signatures = True
    elif option in ['-f', '--file']:
        print_warn("\t[+] Input file = " + str(argument), silent)
        name = str(argument)
    elif option in ['-h', '--help']:
        prints("[+] Help: ", silent)
        merubacc_help()
if exists(name):
    prints("[+] Reading program", silent)
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
        prints("[-] Comparing", silent)
        positives = compare_program(program=program, path=signatures_path, python_exec=python_exec,
                                    both_signatures=both_signatures, iteration=0)
    else:
        if mode == "compress-only":
            prints("[-] Compressing", silent)
            programs = compress_program(program=program, python_exec=python_exec)
        else:
            programs, positives = compress_and_compare_program(program=program, path=signatures_path,
                                                               python_exec=python_exec, both_signatures=both_signatures)
        num_program = 1
        prints("[+] Writing compressed programs", silent)
        for program in programs:
            prints("\t[-] Writing program v." + str(num_program), silent)
            name = name_without_extension + "-" + str(num_program) + "." + extension
            write_program(name=name, att_syntax=att_syntax,
                          program=program)
            num_program = num_program + 1
    if len(positives) > 0:
        if not positives_file or positives_file is None:
            print_pass("Positives: " + str(positives), False)
        else:
            try:
                pfile = open(positives_file, mode="a")
                pfile.write(str(positives))
                pfile.close()
            except getopt.GetoptError as error:
                print(error)
                print_error(
                    "\t[!] Could not print in given file. Check -O or --positives_output argument."
                    "Printing now in standard output.",
                    silent)
                print_pass("Positives: " + str(positives), False)
    sys.exit(0)
else:
    print_error("\t[!] Input file does not exist or is not specified. Check -f or --file argument and try again.",
                silent)
    sys.exit(3)
