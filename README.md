# Make assembly magic great again!

Modern optimizing C++ compilers made assembly language almost obsolete. I don't wrote assembler code for 20 years. Nor I see it used anywhere, except for Intel own libraries.

But C++ optimizers aren't the silver bullet. Each time I write high-optimized algorithm, I go through fight against compilers. I know from the start an assembler code I want to see, but it's hard to force compiler to generate exactly what I need. Modern compilers feel themselves too smart and move code around, allocate registers at their own discretion, and select assembler instructions what they prefer.

Nevertheless, I don't write my own asm code for a few reasons:
- complexity: it may be function with 50 commands, of those only 20 are in main loop. Writing assembler commands is boring by itself, and writing all 50 instructions (while our point of interest is only 20 ones) in low-level assembly language is even more boring
- portability: we have to support various CPU architectures (x86, x64, ARM...), function call ABIs and compiler mangling conventions

