# Cancon

Cancon is a very simple statically-typed concatenative language with type inference. This is a simple type checker and interpreter for it, written in Haskell.

## The Language

### Concatenative Programming

This is a concatenative language, which means that the programming is done not by applying functions to arguments (as is the case in most languages), but by composing functions together. Composing functions just means that they are applied sequentially one after another: `f composed with g` means  `f (g x)` (in Haskell-style notation) or `f(g(x))` in maths- or C-style notation. In concatenative programming, `g f` usually means `f composed with g`, so that the functions are written in the order they are called (and so data flows from left-to-right).

An alternative but equivalent way of thinking about this is that the functions are executed sequentially, taking their arguments from a stack and returning their arguments to the stack.

A much more comprehensive introduction to concatenative programming can be found in [Why Concatenative Programming Matters](https://evincarofautumn.blogspot.ch/2012/02/why-concatenative-programming-matters.html) by Jon Purdy.

### Semantics

Currently the only things supported by Cancon are composing functions (as described above), and quoting expressions.

Quoting an expression turns it into a function which takes no arguments, and returns the original expression (which is the same as pushing the expression onto the stack). This allows higher-order programming, so functions can be passed to and from other functions.

At the moment, only primitive functions and quoted expressions can be composed; there is no way to define your own functions.

Cancon is statically-typed, and supports polymorphism over arguments and the stack. The type of a program can be completely inferred, so no type annotations are needed.

Function types are usually polymorphic with respect to the stack (that is, they will take from/return to the top of the stack, and the state of the rest of the stack doesn't affect them). This is expressed by representing the stack as a product type (or tuple type): `S x Int` represents a stack S (where S is a type variable that can correspond to any stack), with an integer on top.

So an addition function would have the type `S x Int x Int -> S x Int`, which means that it takes a stack with 2 integers on top, and returns the stack with the two integers removed, and with a single (possibly new) integer on top.

A literal value is represented by a function which pushes the value onto the stack. So the literal `2` would have the type `S -> S x Int`: it takes any stack, and returns that same stack with an integer on top.

### Syntax

The syntax is very simple. Identifiers are just strings of letters, and identifiers separated by a space will be composed together.

```
drop drop dup
-- will drop the top 2 values of the stack, then duplicate the one that remains on the top of the new stack
```

To quote an expression, just wrap it in square brackets.

```
[id]
-- pushes id (the identity function) onto the top of the stack
```

Identifiers can also be concatenated inside quotations.

```
[swap drop apply]
-- pushes 'swap drop apply' onto the stack
```


And quotations act just like other functions, so they can be composed like normal.

```
[apply] drop
-- pushes apply onto the stack, then drops it (returning the same stack, so this is the same as the identity function id
```

## Examples

The file Examples.idr contains some example programs. `interpret` parses and type-checks a program (giving an unhelpful error if type checking fails) and then, if it is well-typed, runs it with an empty initial stack. The final state of the stack will be returned.
