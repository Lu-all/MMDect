# Metamorphic rules based code compressor

Code based in [MetaSign](https://github.com/LabSPY-univr/MetaSign) metamorphic rules in compression mode.

Input file must have a "_start" tag at the beginning of code. Currently, .data section is not supported.

Use:

```bash
python main.py -h -i -t -f file -o name
```

- -h or --help to display this guide\
- -i or --intel_output to write the output file in intel syntax (ATT syntax is selected by default).
- -t or --tag_replacement to use an experimental substitution of tags to line number while applying rules.
- -f or --file to specify input file. If not specified, it will use examples/passwddump.txt as input.
- -o or --output to specify name of output file. If not specified, it will be < file >-compressed.< extension >.

For example:

```bash
python main.py -i --file=examples\passwddump.txt -o examples\test.txt
```