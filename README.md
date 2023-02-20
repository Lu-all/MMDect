# Metamorphic Rules Based code Compressor and Comparation

Code based in [MetaSign](https://github.com/LabSPY-univr/MetaSign) metamorphic rules in compression mode.

## Requirements

- Python
- SWI Prolog (optional)
- Input file must have a "_start" tag at the beginning of code.
- .data section must be before "_start" tag
- Input file must be in Intel format

## How to use it

```bash
python merubacc.py -h -a -m {compress-only, compare-only, both} -d -p {none,both,compression,comparation} -f file -o name -s signatures_path
```

- -h or --help to display options.
- -v or --verbose to show output (true by default).
- -a or --att_syntax to write the output file in ATT syntax (Intel syntax is selected by default).
- -m or --mode to specify mode between: compress-only (only execute compression module), compare-only (only execute
  comparation module or both (execute both modules). Both is selected by default.
- -p or --python to execute compression, comparation or both in Python instead of Prolog (default value is none).
- -r or --regex to enable both Regex and Prolog comparation. This option overwrites -p / --python argument.
- -f or --file to specify input file. If not specified, it will use examples/passwddump.txt as input.
- -o or --output to specify name of output file. If not specified, it will be < file >-compressed.< extension >.
- -O or --positives_output to write positives to a file. If not specified, positives will be printed in standard
  output (even in silent mode).
- -s or --signatures to specify path of signatures parent directory, which also enables compare step. Rules for prolog
  calculation should have '.prologsign' extension, while rules extension for comparation in python must be '.txt'.
  Python rules can be in regex format.

For example:

```bash
python merubacc.py -a
```

## Uses

It has two principal uses:

### Compression

This code can be used to reduce the lines of a program, improving its readability.
Arguments are categorized in "Mem" (memory), "Imm" (immediate), "Reg" (register) or others.
When one or more consecutive lines match a rule, those lines are replaced to a simple version. For example:

"MOV [r13], r12"; "PUSH [r13]" can be replaced to "PUSH r12". This can be expressed as

MOV Mem,Imm / PUSH Mem <--> PUSH Imm

In python mode, only the shortest version will be put in a file. However, in default mode (prolog), each possible
compression will be outputted.

You can output the result in Intel syntax or in ATT syntax. By default, ATT syntax is selected.

For example:

```bash
python merubacc.py --file=examples\passwddump.txt --output=examples\test.txt --mode=compress-only
```

### Comparation

This code also can be used to compare instructions between a given program and signatures. If only-compare is not
specified, each compressed version of a program obtained in compression step will be compared.

In python mode, signatures will be compared using regex. Regex signatures must have '.txt' extension.
In prolog mode (default), signatures will be compared as Functors. Prolog signatures must have '.prologsign' extension.

Both signatures type can be compared if -r / --regex argument is specified.

| Regex signature                                   | Prolog signature                         |
|---------------------------------------------------|------------------------------------------|
| .* 'mov',\s*'[re]\w\w?',\s*'0x6477737361702FFF'.* | mov(reg(_Reg),imm('0x6477737361702FFF')) |

For example:

```bash
python merubacc.py -r --file=examples\passwddump.txt --signatures=example_signatures/ --mode=compare-only
```
