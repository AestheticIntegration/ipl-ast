# Imandra Protocol Language OCaml AST Types

Use this OCaml Ast and printer to create IPL AST elements for analysis. E.g.
```
let program = [Import "FIX_4_4"]
```
as an example ast element. Then print to ipl code as string using for example:
```
let ipl_code = program_pp CCFormat.str_formatter program; CCFormat.flush_str_formatter ();;
```

## Building and formatting code

Run
```
make build
```
to build the project;
```
make format
```
to format the code
```
and
```
make clean
```
to clean the build.
