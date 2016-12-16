# reference-compiler

## Scoped functions

Scoped functions cannot access state of outer function; effectively, these are pure old functions that can be access only inside outer function.

So, effective solution is:

0. Every function has it's own context of defined functions.
0. From syntax, all functions are actually defined inside of `main`.
0. While interpretting, interpreter keeps track of available functions.
0. In x86, functions will have names like this: `(<outer_name>.)*fun_name`,
   so function `bar` inside `foo` will have name `main.bar.foo`.
0. For implementation, only stack compiler has to change function names. x86 does not need any changes :).
