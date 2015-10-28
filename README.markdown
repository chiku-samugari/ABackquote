# ABackquote -- an Extended Sharp-Backquote

ABackquote is a reader macro that generates a lambda form whose body is
the subsequent form, as if that form is backquoted. Naturally, commas
are valid in the subsequent form. In addition, all token that is a
member of following regular language are considered to be anaphora in
the form and will be parameters of the lambda form.

    [aA][!]*0 | [aA][!]*[1-9][0-9]*

Before dig into the detail of ABackquote, let me list some examples. It
is assumed that ABackquote is set to `#``. This assumption is used
throughout this document.

    ; simplest use case
    #`(list ,a0 a1)" => (LAMBDA (A0 A1) `(LIST ,A0 A1))

    ; with an integer infix argument
    #3`(list ,a0 ,a2) => (LAMBDA (A0 A1 A2) `(LIST ,A0 ,A2))

    ; anaphora is sorted by the suffix
    #`(list ,a1 ,a0) => (LAMBDA (A0 A1) `(LIST ,A1 ,A0))

    ; suffix can be skipped
    #`(list ,a0 ,a2) => (LAMBDA (A0 A2) `(LIST ,A0 ,A2))

    ; nested ABackquote and shared anaphora
    #`(list ,a0 #`(foo ,a!1 ,a0 #`(baz a!!2)))
    => (LAMBDA (A0 A1 A2)
         `(LIST ,A0 (LAMBDA (A0)
                      `(FOO ,A1 ,A0 (LAMBDA () `(BAZ A2))))))

## Function ENABLE-ABACKQUOTE

    enable-abackquote (&optional (disp-char #\#) (sub-char #\`)) => t
    disp-char --- a character. The default is #\#.
    sub-char --- a character. The default is #\`.

enable-abackquote makes disp-char followed by sub-char to be a
ABackquote reader macro.

## ABackquote detail

ABackquote is an extension of Sharp-Backquote reader macro, which is
introduced in the first edition of Doug Hoyte's _Let Over the Lambda_
section 5. As the truth, under the same use cases, Hoyte's
Sharp-Backquote and ABackquote behaves in an identical manner, except
the starting value of suffix; the suffix of anaphora in
ABackquote starts from 0, while that starts from 1 in Sharp-Backquote.

### Behaviour

ABackquote is a reader macro that generates a lambda form whose body is
the subsequent form, as if that form is backquoted. Therefore, commas
are valid in the subsequent form. The strategy to build the lambda list
of the resulting lambda form depends on the optional integer infix
argument.

If the integer argument `n` is given, it is recognized as the number of
parameters. Even if some of introduced parameters do not appear in the
expression, all parameters are introduced. The name of parameters are
`Ai` where `i` is an integer from `0` upto `n - 1`. The first parameter
is `A0`, the second is `A1`, and so on.

    #3`(list ,a0 ,a2) => (LAMBDA (A0 A1 A2) `(LIST ,A0 ,A2))

Here, `=>` means "if the left hand side is read, then right hand side
will be generated." Practically, the right hand side will be EVALuated
subsequently and becomes an object whose type is function.

This type of automatically introduced parameters can be thought as one
variant of anaphora. It is called anaphora in this document.

Anaphora are interned to the current package of read time.

If the integer infix argument is not given, on the other hand, all the
tokens that are members of following regular language are considered to
be parameters of resulting lambda form.

    [aA][!]*0 | [aA][!]*[1-9][0-9]*

Those tokens are collected, interned and sorted based on the decimal
integer suffix.

    #`(list ,a7 ,a0 ,a1) => (LAMBDA (A0 A1 A7) `(LIST ,A7 ,A0 A1))

As it is shown, anaphora which does not appear in the subsequent form is
not introduced in this case.

There are 2 major differences between the original Sharp-Backquote and
ABackquote. First, the infix integer argument is optional even in the
use of multiple anaphora. This aspect is explained above. Second,
ABackquote offeres special kind of anaphora named *shared anaphora*, 
which are shared with outer level ABackquote forms.

An anaphor appeared within nested ABackquote forms is normally
introduced by the innermost ABackquote form that includes the anophor.

    #`(list ,a0 #`(list ,a0 ,a1))
    => (LAMBDA (A0) `(LIST ,A0 (LAMBDA (A0 A1) `(LIST ,A0 ,A1))))

Shared anaphora are anaphora that we can control which of nested
ABackquotes introduces that anaphor. In the implementation, the number
of exclamation marks (!) included in an anaphor token controls the place
where the anaphor is introduced. Each anaphor is introduced by the *n*
levels outer ABackquote than the innermost ABackquote for the anophor,
where *n* is the number of exclamation marks in the anophor token.

    #`(list ,a1 ,a0 #`(foo ,a0 ,a!2 #`(bar ,a!0 ,a!!2)))
    => (LAMBDA (A0 A1 A2)
         `(LIST ,A1 ,A0 (LAMBDA (A0)
                          `(FOO ,A0 ,A2 (LAMBDA () `(BAR ,A0 ,A2))))))

Conceptually, the normal anaphora like `A0` are considered to be shared
anaphora which has *0* exclamation marks. It means that 0 levels outer
ABackquote introduces that anaphora and 0 levels outer means the
innermost.

The name of shared anaphora do not include exclamation marks as it is
shown in the example above. This is reasonable because the token for
same shared anaphora can differ if the location is different. `A2` of
above example is the case.

### Combination with other reader macros

ABackquote form cannot work with reader macros which are intended to
globally modifies the readtable because it unwinds the readtable to the
value of `*readtable*` when the read of ABackquote form ends.

In addition, ABackquote uses #\a and #\A as reader macros to pick
anphora up. Therefore, the reader macros for #\a and #\A set by the user
will never be invoked during the ABackquote form reading.

### Delimiter characters

All macro characters and characters whose character syntax type is
*whitespace[2]* works as the delimiters for a token starts with #\a or
 #\A within ABackquote form. Not only terminating macro characters but
also NON-terminating macro characters are working as the delimiter
characters because I could not find out a standard method that tells if
a macro charcter is a terminating macro character or non-terminating
macro character.

For example, let `#\?` be a non-terminating macro character which returns
no values. We will get following result.

    (set-macro-character #\? (lambda (strm c) (values)) t) ;=> T

    (read-from-string "#`(list ,a0?b)")
    ;=> (LAMBDA (A0) `(LIST ,A0 B))

I want to know how to avoid this situation.

## Consideration

### Anaphora token specification

As it has already explained, tokens that are members of following
regular language is considered as anaphora.

    [aA][!]*0 | [aA][!]*[1-9][0-9]*

Please be aware that this syntax for anaphora specifies which *token*
will be an anophor. It does not specifies the syntax of symbol names of
symbols which are read.

For instance, if `a\1` appears as a part of source code, then usually it
will be read as a symbol whose SYMBOL-NAME is `A1`. However, the token
violates the syntax of anaphora and it will not be considered as an
anophor even if it appears within an ABackquote form.

The situation will be complicated a bit if both of `a0` and `a\0`
appears in an ABackquote form. The resulting lambda form will include an
anphor whose name is `A0` because `a0` appears, and the result of INTERN
for the token `a\1` also refers that parameter.

    #`(list ,a0 ,a\0) => (LAMBDA (A0) `(LIST ,A0 ,A0))

Similar discussion stands for the package marker, too. These seem to
be confusing and might be changed.

Another component of READ that could affect to the behaviour of
ABackquote is `*READ-BASE*`. It is ignored. Even if the `*READ-BASE` is
larger than 10, a token `a0` will be an anphor within ABackquote form.
Similarly, even if the `*READ-BASE*` is 4, for example, `a10` will be
interned as the symbol whose SYMBOL-NAME is `A10` not `A4`.

Suffix of each anaphor is intepreted as an integer value and used as the
key to sort anaphora. It means, `A20` comes later than `A3`. I believe
it is what we expect.

Padding 0 is not allowed for suffix because the result of anaphora
sorting is not well predictable for human being.

### Abuse of shared anaphora

Abuse of shared anaphora should be avoided.

This feature is implemented to show it is possible, and to keep the
exclamation semantics consitent. Consistency is quite important for
myself.

The idea, the number of exclamation marks decides the level of
ABackquote which introduces the parameter, is taken from the behaviour
of commas in the backquote reader macro of Common Lisp. Although the
effect of this exclamation mark is far more easier to predict, we have
already experienced a lot of confusion when we abuse this kind of syntax
sugars.

## Known issues

## Shared anaphora with infix integer argument

Not working at all for now.

## Too many exclamation marks

Likewise commas in a backquote form, too many exclamation marks should
be handled by ABackquote. It is not handled for now.

## miscellaneous

### Parameters vs. Arguments

A terminology *integer infix argument* is used in this document. This is
actually called integer infix *parameter* in the specification document
of ANSI Common Lisp. I think the parameter is the receiver of arguments.
Integer infix *parameter* will receive integer infix *argument*.
Therefore, a number written between sharp and backquote
should be called *argument*, I think.

## Author and License

Author : chiku (Takehiko Nawata, samugari.penguin@gmail.com)

License : MIT License
