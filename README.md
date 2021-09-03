# pascal

A Pascal compiler

Usage:
	pascal source.p

The source file can have either a .p or .pas extension.

Input:

ISO 7185 Level 0 (no conformant-array parameters) Pascal
While the standard was used for design, I do not claim this compiler conforms to the standard.

There are a few extension to the language that I will enumerate later.

Output:

GNU gas assembly language.

x86_64 with AVX instructions.

If you don't use set types you won't need the AVX instructions.

The generated output is OS agnostic. However the library routines (available elseware) is only for Linux.



