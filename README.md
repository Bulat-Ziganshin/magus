I thank Eugene Shelwien and Dmitry Bortoq for long discussions regarding these topics. Many key ideas of this project were proposed by them, and without their help, it will never became reality.


# Make assembly magic great again!

Modern optimizing C++ compilers made assembly language almost obsolete. I don't wrote assembly code for 20 years. Neither I see it used anywhere, except in Intel own libraries.

But C++ optimizers aren't the silver bullet. Each time I write high-optimized algorithm, I go through fight against compilers. I know from the start an assembler code I want to see, but it's hard to force compiler to generate exactly what I need. Modern compilers feel themselves too smart and move code around, allocate registers at their own discretion, and select assembler instructions what they prefer.

Nevertheless, I don't write my own asm code for a few reasons:
- portability: we have to support various CPU architectures (x86, x64, ARM...), object/library formats, calling conventions and name mangling conventions
- brevity: it may be function with 50 commands, of those only 20 are in main loop. Writing assembler commands is boring by itself, and writing all 50 instructions (while our point of interest is only 20 ones) in low-level way is even more boring


## Portability

Fortunately, there are various solutions to both classes of problems, in particular for portability:
- name mangling and object/library formats can be converted by Agner Fog [objconv] utility. It's especially important, since it means that all speed-critical code can be compiled by the same best compiler (GCC) and then linked into other compiler executables!
- calling convention (ABI) portability falls into 3 classes:
  - all x86 code can be made compatible by using `cdecl` declaration
  - x64 Windows compilers have the single ABI
  - x64 Unix compilers also have the single ABI, even if incompatible with the Windows one
- x86 and x64 code is almost compatible. The main differences are pointer width and number of registers available. Both can be somewhat solved using symbolic register names, f.e. `ptr8` can be translated into either `r8` or `[esp+4*8]` depending on CPU

Overall, we can make assembly code fully portable within single ISA, and highly portable between x86 and x64, using macro packages to abstract register names and to hide ABI. Alternatively, high-quality code produced by GCC can be linked into executables produced by other compilers.


## Brevity

### [High-level assemblers]

Various approaches to make assembly code more compact and readable are available in MASM, [FASM], [HLA], [ForwardCom], and have the common name of [High-level assemblers]. A few examples:
- function/call facility which hides ABI and simplifies/reduces code
- C-style instruction syntax, a la `EAX += EBX`
- structured compound statements (if, while...), sometimes with relational operations (`if EAX > EBX`)
- complex expressions for assignments and if/while conditions (`if EAX*2 > EBX`)
- typeful register declarations and endless virtual registers, where extra registers are spilled into stack

PTX is a particularly interesting example of high-level virtual assembly language that allows to declare unlimited amount of typed "registers" and supports legacy ISA instructions by emulating them with command sequences. This allows NVidia to make each next generation of video cards incompatible with previous one, and to adapt to varying amount of registers that depends on compilation options.


### Sphinx C--




















[objconv]: http://www.agner.org/optimize/#objconv
[FASM]: https://en.wikipedia.org/wiki/FASM
[HLA]: https://en.wikipedia.org/wiki/High_Level_Assembly
[ForwardCom]: https://github.com/ForwardCom/code-examples
[High-level assemblers]: https://en.wikipedia.org/wiki/High-level_assembler
