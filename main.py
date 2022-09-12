import sys

from IOHandler import read_program, write_program
from compressor import compress_program

att_syntax = False
tag_replacement = False
if len(sys.argv) == 3:
    # Output program in att syntax
    att_syntax = sys.argv[2] == str(1)
elif len(sys.argv) == 4:
    # tag replacement for numbers (experimental)
    att_syntax = sys.argv[2] == str(1)
    tag_replacement = sys.argv[3] == str(1)
elif len(sys.argv) < 2 | len(sys.argv) > 4:
    print("Error: incorrect number of parameters")
    sys.exit()
name = str(sys.argv[1])
name_without_extension = str(sys.argv[1].split('.')[0])
program = read_program(name=name, tag_replace_to_numbers=tag_replacement)
program = compress_program(program)
write_program(name=name_without_extension + "-compressed.txt", att_syntax=att_syntax,
              program=program, use_tag_replacement=tag_replacement)
