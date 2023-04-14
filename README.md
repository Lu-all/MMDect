# Metamorphic Rules Based code Compressor and Comparison

Code based in [MetaSign](https://github.com/LabSPY-univr/MetaSign) metamorphic rules in compression mode.

## Requirements

- Python
- SWI Prolog (optional)
- Input file must have a "_start" tag at the beginning of code.
- .data section must be before "_start" tag
- Input file must be in Intel format

## How to use it

```bash
python merubacc.py -h -a -m {compress-only, compare-only, both} -d -p {none,both,compression,comparison} -f file -o name -s signatures_path
```

- -h or --help to display options.
- -v or --verbose to show output (true by default).
- -a or --att_syntax to write the output file in ATT syntax (Intel syntax is selected by default).
- -m or --mode to specify mode between: compress-only (only execute compression module), compare-only (only execute
  comparison module or both (execute both modules). Both is selected by default.
- -p or --python to execute compression, comparison or both in Python instead of Prolog (default value is none).
- -f or --file to specify input file. If not specified, it will use examples/passwddump.txt as input.
- -o or --output to specify name of output file. If not specified, it will be < file >-compressed.< extension >.
- -O or --positives_output to write positives to a file. If not specified, positives will be printed in standard
  output (even in silent mode).
- -s or --signatures to specify path of signatures parent directory, which also enables compare step. Rules for prolog
  calculation should have '.prologsign' extension, while rules extension for comparison in python must be '.txt'.
  Python rules can be in regex format.
- -c or --compare-both to compare both Regex and Prolog signatures (overwrites -p python in comparison).

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

### Comparison

This code also can be used to compare instructions between a given program and signatures. If only-compare is not
specified, each compressed version of a program obtained in compression step will be compared.

In python mode, signatures will be compared using regex. Regex signatures must have '.txt' extension.
In prolog mode (default), signatures will be compared as Functors. Prolog signatures must have '.prologsign' extension.

Both signatures type can be compared if -c / --compare_both argument is specified.

| Regex signature                                   | Prolog signature                         |
|---------------------------------------------------|------------------------------------------|
| .* 'mov',\s*'[re]\w\w?',\s*'0x6477737361702FFF'.* | mov(reg(_Reg),imm('0x6477737361702FFF')) |

For example:

```bash
python merubacc.py -c --file=examples\passwddump.txt --signatures=example_signatures/ --mode=compare-only
```

## How to make Prolog signatures

Prolog signatures are a list of functors.
Each functor is composed in the same way that instructions, i.e. instruction(type(argument1), type(argument2)).
For example, `shr r12, 8` will be `shr(reg(r12), imm('8'))`.

### Types:

- Register / reg(register): r12 <--> reg(r12)
- Immediate / imm(immediate): 8 <--> imm(8)
- Memory / mem('address'): \[r12] <--> mem('r12')
- Tag / tag(name): close_file <--> tag(close_file)

### Variables:

A variable can be defined as V or _V (in this program, values of variables defined in signatures will not be displayed).
For example:

`mov(reg(_Reg),imm('0x6477737361702FFF')),
op(shr,reg(_Reg),imm('8'))`

In this code, _Reg must have the same value in both occurrences.
If the first _Reg is assigned the value r12, the second line must be `op(shr,reg(r12),imm('8'))` to valid the rule.

### Wildcard:

A wildcard whose value will not necessarily be repeated later in the code can be defined as "_".
For example:

`mov(reg(_),imm('0x6477737361702FFF')),
op(shr,reg(_),imm('8'))`

In this code, _ doesn't have to have the same value in both instances.
If the first _ is assigned the value r12, the second line could be `op(shr,reg(r14),imm('8'))`
and the rule would apply.
