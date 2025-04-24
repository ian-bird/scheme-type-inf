# Overview

This is a static type checker for scheme that runs against the classical collection of lisp functions:
1. atom?
2. car
3. cdr
4. cond (we actually are using if here since it's easier to work with)
5. cons
6. eq?
7. label
8. lambda
9. quote

The type checker uses an inference algorithm to deduce the types of
variables from how they're being used, and can generate errors if
they're being used incorrectly. It also works to generate type
signatures for lambdas, with the future expectation that it can be
used to expand the list of functions that the type checker can analyze
and allow for the generation of more optimized code from future
compilers, that is able to omit type checks and arity checks on
function calls when it can be deduced statically that the call is
safe.

# Demo

The type checker prints an error since the deduced type of fn for seq from
`(seq)` clashes with its use as the argument to `cdr`:
```lisp
(type-check '(lambda (seq acc)
               (if (eq? '() seq) 
			       acc
				   (reverse (cdr seq) 
				            (cons (seq) acc)))))

; WARNING: type error: (reverse (cdr ?seq) (cons (?seq) ?acc))
; => (((?seq nil) (?acc #:g201) (?result #:g201)))							
;
; note: gensyms represent "any", as in, any type is valid when checked against them.
```
nevertheless, a signature is generated since as long as seq is nil, the type error can
be avoided, any any caller that satisifies that signature can be guaranteed to not 
encounter a type error at runtime.

When we run it again after fixing the type error we get the following response:
```lisp
(type-check '(lambda (seq acc) 
               (if (eq? '() seq) 
			       acc
				   (reverse (cdr seq) 
				            (cons (car seq) acc)))))
; => (((?seq pair)
;      (?acc #:g420)
;      (?result #:g419))
;     ((?seq nil)
;      (?acc #:g421)
;      (?result #:g421)))
```

In this case, two valid type signatures are generated, to account for
the valid possible types for seq. This format was seleected since it
allows for easier checking of arguments and extension of the type checker in the future.

# Plans

I'm at a bit of a roadblock with this for now, it's at a point where
it's capable of actually deducing types, but relying on them seems a
bit risky. The fact that the entire signature of a function can be
redefined at runtime in a completely undecideable manner (i.e. a user
defining new functions through a repl) means that redefining cons, for
instance, could force every single compiled function to have to
recompile to determine if it needs to switch between a dynamically
checked or an unsafe call to the function.

This massive performance hit is obviously unacceptable, but it seems
like the only way to allow for it would be to settle for some kind of
time stamping of when functions where generated and whether the
function can still safely make a call without dynamic checks, and at
that point doing the dynamic check is probably cheaper. I have some
reading to do I think before deciding if this path is even worth
continuing to pursue as an avenue to building a high performance
compiler.
