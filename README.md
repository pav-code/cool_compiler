# COOL Compiler

Compiler based on skeleton provided by Stanford university. COOL stands for classroom object oriented language. The generated code is for MIPS microprocessor. Tools used for assignments 1 and 2 are FLEX and Bison, respectively.

# Testing

Precompiled binaries are included in the "Test" folder. Running the Linux bash script as follows, where the first line runs the lexer, parser, semantic analyzer and code generator (in that order) and outputs the generated MIPS assembly file, indicated by the .s extension. The second step is evoking the spim emulator (must be installed on the system) on that file and the output of the program is outputted to terminal:

  ./mycoolc <test cool file>.cl           [./mycoolc Examples/hello_world.cl]
  
  spim -file <test cool file>.s           [spim -file hello_world.s]
  
  example output:
  SPIM Version 6.5 of January 4, 2003
  Copyright 1990-2003 by James R. Larus (larus@cs.wisc.edu).
  All Rights Reserved.
  See the file README for a full copyright notice.
  Loaded: /usr/class/cs143/cool/lib/trap.handler
  Hello, World.
  COOL program successfully executed
  Stats -- #instructions : 168
         #reads : 27  #writes 22  #branches 28  #other 91
