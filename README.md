I thank Eugene Shelwien and Dmitry Bortoq for long discussions regarding these topics. Many key ideas of this project were proposed by them, and without their help, it will never became reality.


# Make assembly magic great again!

Modern optimizing C++ compilers almost entirely displaced assembly languages. I don't wrote assembly code for 20 years. Neither I see it used anywhere, except for Intel own libraries.

But C++ optimizers aren't the silver bullet. Each time I write high-optimized algorithm, I go through fight against compilers. I know from the start an assembler code I want to see, but it's hard to force compiler to generate exactly what I need. Modern compilers feel themselves so smart and move code around, allocate registers at their own discretion, and select assembler instructions what they prefer.

Nevertheless, I don't write my own asm code for a few reasons:
- portability: we have to support various CPU architectures (x86, x64, ARM...), object/library formats, calling conventions and name mangling conventions
- brevity: a function may have 50 commands, of those only 20 are in main loop. Writing assembler commands is boring by itself, and writing all 50 instructions (while our point of interest is only 20 ones) in low-level way is even more boring


## Portability

Fortunately, there are various solutions to both classes of problems, in particular for portability:
- name mangling and object/library formats can be converted by Agner Fog [objconv] utility. It's especially important, since it means that all speed-critical code can be compiled by the same best compiler (GCC) and then linked into executables produced by inferior compilers!
- calling convention (ABI) portability falls into 3 classes:
  - all x86 code can be made compatible by using `cdecl` declaration
  - x64 Windows compilers have the single ABI
  - x64 Unix compilers also have the single ABI, even if incompatible with the Windows one
- x86 and x64 code is almost compatible. The main differences are pointer width and number of registers available. Both can be somewhat solved using symbolic register names, f.e. `ptr_reg8` can be translated into either `r8` or `[esp+4*8]` depending on CPU

Overall, we can make assembly code fully portable within single ISA, and highly portable between x86 and x64, using macro packages to abstract register names and to hide ABI. Alternative approach is to link high-quality code produced by GCC into executables produced by other compilers.


## Brevity

### [High-level assemblers]

Various approaches to make assembly code more compact and readable are available in MASM, [FASM], [HLA], [ForwardCom], and have the common name of [High-level assemblers]. A few examples:
- function/call facility which hides ABI and simplifies/reduces code
- C-style instruction syntax, a la `EAX += EBX`
- structured compound statements (if, while...), sometimes with relational operations (`if EAX > EBX`)
- complex expressions for assignments and if/while conditions (`if EAX*2 > EBX`)
- typeful register declarations and endless virtual registers, where extra registers are spilled into stack

PTX is a particularly interesting example of virtual assembly language that allows to declare unlimited amount of typed "registers" and supports legacy ISA instructions by emulating them with command sequences. This allows NVidia to make each next generation of video cards incompatible with previous one, and to adapt to varying amount of registers that depends on compilation options.


### Sphinx C--

Sphinx C-- is the language providing C-like syntax for assembly code, including computations and if/while statements. The rest is implemented via usual assembly statements. It looks like ideal high-level assembly language for me, but unfortunately original compiler was 16-bit only and various 32/64-bit clones don't took off.

So my first idea was to make open-source implementation of similar language using a modern parsing approach (such as PEG) in a high-level language (probably, Haskell or OCaml) with massive extensibility features (ability to add new operators and statements).


### Turbo: C with benefits

And at this moment I recalled Turbo C - old dumb C compiler that allowed to use register names as variables, f.e. `if (_AX > _BX)  _AX <<= _CL` plus had plain MS-style asm inline statements. These two features made it quite similar to Sphinx C--, but with important benefit - except for these two extensions, it was plain C code. This allowed to write code that contains both portable and optimized low-level implementation, selected depending on the compiler used:

```C
#if TURBO_C
#  define bitbuf _AX
#  define count  _CL
#else
   int bitbuf, count;
#endif

   bitbuf <<= count;
```

My experience of program optimization using Turbo C was really nice - I started with existing C algorithm and gradually replaced all complex expressions with single-operation assignments, and then added register bindings, similar to the code above, to declarations of hot variables. And the code remained working at each step of this transformation.

This pseudo-variable register syntax is probably available in newer Borland C++ compilers too, including free Borland C++ 5.5 version.


### New ideal found

At this point I realized that all I need is just C/C++ "with benefits":
- compiler shouldn't reorder statements
- support for manual and semi-automatic assignment of concrete registers to variables - use pragmas ignored by usual C/C++ compilers
- all asm commands can be generated via intrinsics - provide equivalent implementation in plain C/C++

Similar to Turbo C approach, it will allow to develop plain C/C++ code and debug it using any existing C/C++ compiler. Once the code is working, we can rewrite critical loops to use only low-level operations, directly compilable to single assembly commands. At any moment, it still remains usual C/C++ code whose correctness can be checked with usual C/C++ compiler.

Once transformation is done, we can compile the code with our Magus C++ compiler and get exactly the asm code we developed. On platforms not supported by Magus, the code still can be compiled with usual C/C++ compilers.

This approach will provide us all benefits of Sphinx C-- (i.e. code portability, brevity and familiar C syntax), plus allow to share the same code between C/C++ (for portability to any system and debugging) and high-level assembler (for performance).

Now, once I figured what to do, I started to research various approaches to C/C++ compiling which can be extended with Magus code generator: LCC, TCC, ANTLR C++ parser, gcc/clang IR transformations. But every approach I was able to find was either hard to learn and implement (such as IR transformations), or had limited usefulness (such as modification of any OSS C compiler), so while my goal became perfectly defined, implementation seemed pretty hard.

Another variation of this idea was employing Nim - it allows to transform code AST at the compile time, which is exactly a kind of transformation I'm looking for. So, once algorithm was developed as low-level Nim code, it can be translated into C/C++ in usual way, or preprocessed by Magus replacing original statements with inline asm code.


### Embedding

And at this point of discussion Eugene brought two great ideas to the table:
- we don't need to produce asm code directly, instead we can generate gcc inline `asm` statements, which then can be compiled by any major C/C++ compiler (except for MSVC)
- we don't need to parse and process entire input file, instead we can translate only specifically marked regions. This makes principal difference, freeing us from the burden of full C++ language support. Instead, we need to support only language subset used in the statements, and moreover - only part of the whole statement syntax that we find really useful for this type of HPC computing. And even this small C subset can be implemented incrementally if we will start with support for generic `asm` statement.

And combination of these ideas is absolute win, allowing us to quickly develop minimal practical translator and then extend it at comfortable pace. Since we plan to stay strictly within existing C/C++ syntax, and don't need anything but raw syntax parser, we can choose among well-known existing C/C++ parsers, such as [ANTLR C++14] and [Haskell C-language]. C-language parser has additional advantage - it supports gcc `asm` statements in the AST type, and provides AST pretty-printer, so we can map C statements like `eax=ebx` into corresponding asm statements and then pretty-print code region back to feed any C/C++ compiler compatible with GCC asm statements.


### Implementation

Implementation plan is:
- [x] Magus inputs a sequence of GCC C statements and outputs compound GCC C statement that includes GCC-style asm statements.
- [x] all identifiers are translated as register names, i.e. `EAX` -> `%%EAX`.
- [x] all function calls are translated as asm commands with the same name, i.e. `CRC32(EAX,1)` -> `CRC32 1, %%EAX` and `EBX = CRC32(EAX,1)` -> `MOV %%EAX, %%EBX; CRC32 1, %%EBX`.
- [ ] map `ax += bx` to inline asm code
- [ ] `ax = bx+cx`
- [ ] `goto lbl; lbl:`
- [ ] `if (CF)  goto lbl`
- [ ] `if (ax < bx)  goto lbl`
- [ ] `{}`
- [ ] `if {}`
- [ ] `while`
- [ ] `for`
- [ ] gcc-compatible intrinsic set
- [ ] manual and semi-automatic register allocation
- [ ] complex expressions, and allocation of temporary registers to handle that






















[objconv]: http://www.agner.org/optimize/#objconv
[FASM]: https://en.wikipedia.org/wiki/FASM
[HLA]: https://en.wikipedia.org/wiki/High_Level_Assembly
[ForwardCom]: https://github.com/ForwardCom/code-examples
[High-level assemblers]: https://en.wikipedia.org/wiki/High-level_assembler
[ANTLR C++14]: https://github.com/antlr/grammars-v4/tree/master/cpp
[Haskell C-language]: http://hackage.haskell.org/package/language-c
