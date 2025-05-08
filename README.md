# SCARY, the SCheme ARraY Language

SCARY is an array programming language embedded in Scheme as the final project of Luca Musk and Raffi Sanna for MIT's 6.5150.

## Front-End

The SCARY frontend language has the following syntax:

```
Expr         := Form | Float | Variable
Op           := + | * | - | / | min | max | < | > | =
Form         := (Op Expr Expr)
              | (length Expr)
              | (ref Expr Expr)
              | (compute ((Variable Expr) ...) Expr)
              | (array Expr Expr ...)
              | (if Expr Expr Expr)
              | (fold Op Expr Expr)
              | (let* ((Variable Expr) ...) Expr)
```

## Middle-End

The SCARY middle-end language has the following syntax:

```
Expr        := Literal | Variable | Func-Call | Index
Func-Call   := (Op Expr Expr) | (length Expr)
Index       := (ref Variable Expr ...)
Init        := (declare Variable Expr ...)
Assignment  := (set! Variable (Expr ...) Expr)
Statement   := Assignment | Init | Conditional | Loop | Return
Return      := (return Expr)
Conditional := (if Expr (Statement ...) (Statement ...))
Loop        := (for Variable Expr_min Expr_max Statement ...)
```

## Back-End
