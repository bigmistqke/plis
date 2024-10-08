# @bigmistqke/plis 🥺👉👈 

Toy lisp interpreter.

Support for:
- `define` 👉 `(define big 69)`
- `fn` 👉 `(fn (a b) (+ a b))`
- `if` 👉 `(if (> x 0) (+ x 1) (- x 1))`
- `let`/`let*` 👉 `(let ((big 69) (mistqke 420)) (add big mistqke))`
- `quote`/`quasiquote`/`unquote` 👉 `'(1 2 3)` `(quasiquote (1 ,(+ 1 1), 3))` 

## How To Use

```tsx
import { Cons, createStandardEnvironment, evaluate, interpret, parse, tokenize } from "@bigmistqke/plis"

// Interpret a single line
interpret("(+ 1 2)")

// Add custom environment
const environment = createStandardEnvironment()
environment.add("uppercase", (arg: Cons) => { return arg.value.value.toUppercase() })
interpret("(uppercase bigmistqke)", environment)

// Chain multiple lines with shared environment
interpret
  .pipe("(define big 69)")
  .pipe("(define mistqke 420)")
  .pipe("(define add (fn (a b) (+ a b)))")
  .return("(add big mistqke)")

// Parse and evaluate AST
const ast = parse(tokenize("(+ 1 2)"))
evaluate(ast)
```

## Repl

https://github.com/user-attachments/assets/75bfcea2-3ddb-4fa5-94c7-cec0663637c4

## License

MIT &copy; [bigmistqke](https://github.com/sponsors/bigmistqke)
