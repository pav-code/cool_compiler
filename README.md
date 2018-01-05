# COOL Compiler

Compiler based on skeleton provided by Stanford university. COOL stands for classroom object oriented language. The generated code is for MIPS microprocessor. Tools used for assignments 1 and 2 are FLEX and Bison, respectively.

# Testing

Precompiled binaries are included in the "Test" folder. Running the Linux bash script as follows, where the first line runs the lexer, parser, semantic analyzer and code generator (in that order) and outputs some files, most importantly the generated MIPS assembly file with the .s extension. The second step is evoking the spim emulator on that file and the output of the program is outputted to terminal:

  ./mycoolc <test cool file>.cl           [./mycoolc Examples/hello_world.cl]
  
  spim -file <test cool file>.s           [spim -file hello_world.s]
