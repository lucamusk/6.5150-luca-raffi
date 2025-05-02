# SCARY, the SCheme ARraY Language

SCARY is an array programming language embedded in Scheme as the final project of Luca Musk and Raffi Sanna for MIT's 6.5150.

## Front-End

The SCARY frontend language has the following syntax:

```
Literal     := Number
ArrayLit    := (array ArrayLitObj)
ArrayLitObj := Literal | (ArrayLitObj ...)
Expr        := Call | Literal | ArrayLit
Op          := + | * | - | / | Min | Max
Call        := (Op Expr Expr)
             | (filter Expr Expr Expr)
             | (iota Expr)
             | (ref Expr Expr)
             | (fold Op Expr Expr)
             | (reduce Op Expr Expr)
             | (reorder Expr Expr)
             | (lift Expr)
```

## Middle-End

The SCARY middle-end language has the following syntax:

```
Expr        := Literal | Variable | Func-Call | Index
Func-Call   := (Op Expr Expr) | (length Expr)
Index       := (ref Variable Expr ...)
Init        := (declare Variable Expr ...)
Assignment  := (set! Variable (Expr ...) Expr)
Statement   := Read | Write | Assignment | Init | Conditional | Loop
Read        := (read Variable Number)
Write       := (write Expr)
Conditional := (if Expr (Statement ...) (Statement ...))
Loop        := (for Variable Expr_min Expr_max Statement ...)
```

## Back-End
