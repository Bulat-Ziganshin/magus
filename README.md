# Make assembly magic great again!

Modern optimizing C++ compilers made assembly language almost obsolete. I don't wrote assembly code for 20 years. Neither I see it used anywhere, except in Intel own libraries.

But C++ optimizers aren't the silver bullet. Each time I write high-optimized algorithm, I go through fight against compilers. I know from the start an assembler code I want to see, but it's hard to force compiler to generate exactly what I need. Modern compilers feel themselves too smart and move code around, allocate registers at their own discretion, and select assembler instructions what they prefer.

Nevertheless, I don't write my own asm code for a few reasons:
- complexity: it may be function with 50 commands, of those only 20 are in main loop. Writing assembler commands is boring by itself, and writing all 50 instructions (while our point of interest is only 20 ones) in low-level way is even more boring
- portability: we have to support various CPU architectures (x86, x64, ARM...), object/library formats, calling conventions and name mangling conventions


## Portability

Fortunately, there are various solutions to both classes of problems, in particular for portability:
- name mangling and object/library formats can be converted by Agner Fog [objconv] utility. It's especially important, since it means that all speed-critical code can be compiled by the same best compiler (GCC) and then linked into other compiler executables!
- calling convention (ABI) portability falls into 3 classes:
  - all x86 code can be made compatible by using `cdecl` declaration
  - x64 Windows compilers have the single ABI
  - x64 Unix compilers also have the single ABI, even if incompatible with the Windows one
- x86 and x64 code is almost compatible. The main differences are pointer width and number of registers available. Both can be somewhat solved using symbolic register names, f.e. `ptr8` can be translated into either `r8` or `[esp+4*8]` depending on CPU


[objconv]: http://www.agner.org/optimize/#objconv
