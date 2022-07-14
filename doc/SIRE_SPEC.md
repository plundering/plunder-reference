Notes for Sire Specification
============================

Expressions:

=$ (bx nat) natBar bx

#(3 4)

    NAT : 24929 "aa"

    REF : asdf

    RAW : #x

    APP : f|x       -- normal
          f!x       -- inline
          f$x       -- static

    LET : [@ v x b] -- normal
          [@@ v x b] -- recursive

    LAM : (f x)?x   -- normal
          (f x)?!x  -- inline
          (f x)?$x  -- static
          x&x       -- anon, normal
          x&!x      -- anon, normal
          x&$x      -- anon, normal

Commands:

    DUMP     := <x
    PRINT    := x
    DEFUN    := (f x)=3
    DEFINE   := x=3
    ASSERT   := !x
    COMMENT  := ;x
    DEFMACRO := [#= "=*" [? (envr next kids more) ...]]

We print bindings values as:

    x:=(#3)
    x:=(#(3 3))
