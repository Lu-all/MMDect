# Metamorphic rules based code compressor

Code based in [MetaSign](https://github.com/LabSPY-univr/MetaSign) metamorphic rules in compression mode.

Input file must have a "_start" tag at the beginning of code. Currently, .data section is not supported.

Use:

```bash
python main.py <code_to_compress> <att_output>
```

- code_to_compress: input code
- att_output: output code in att format (1=TRUE)

For example:

```bash
python main.py $FileDir$\examples\passwddump.txt 1
```