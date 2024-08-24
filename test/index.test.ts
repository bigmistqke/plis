import { describe, expect, it } from "vitest"
import {
  Cons,
  InvalidArgumentError,
  InvalidExpressionError,
  InvalidFunctionError,
  UndefinedVariableError,
  interpret,
  parse,
  toJson,
  tokenize,
} from "../src"

describe("Tokenization", () => {
  it("should tokenize a simple expression", () => {
    const tokens = tokenize("(+ 1 2)")
    expect(tokens).toEqual(["(", "+", "1", "2", ")"])
  })

  it("should handle nested expressions", () => {
    const tokens = tokenize("(* (+ 1 2) (- 3 4))")
    expect(tokens).toEqual([
      "(",
      "*",
      "(",
      "+",
      "1",
      "2",
      ")",
      "(",
      "-",
      "3",
      "4",
      ")",
      ")",
    ])
  })

  it("should tokenize a define statement", () => {
    const tokens = tokenize("(define x 10)")
    expect(tokens).toEqual(["(", "define", "x", "10", ")"])
  })

  it("should tokenize an empty list", () => {
    const tokens = tokenize("()")
    expect(tokens).toEqual(["(", ")"])
  })
})

describe("Parsing", () => {
  it("should parse a simple expression", () => {
    const ast = parse(tokenize("(+ 1 2)"))
    expect(toJson(ast)).toEqual(["+", 1, 2])
  })

  it("should parse nested expressions", () => {
    const ast = parse(tokenize("(* (+ 1 2) (- 3 4))"))
    expect(toJson(ast)).toEqual(["*", ["+", 1, 2], ["-", 3, 4]])
  })

  it("should parse a define statement", () => {
    const ast = parse(tokenize("(define x 10)"))
    expect(toJson(ast)).toEqual(["define", "x", 10])
  })

  it("should parse a function definition", () => {
    const ast = parse(tokenize("(define square (fn (x) (* x x)))"))
    expect(toJson(ast)).toEqual([
      "define",
      "square",
      ["fn", ["x"], ["*", "x", "x"]],
    ])
  })

  // Adding tests for quote
  it("should parse a quote expression", () => {
    const ast = parse(tokenize("'(1 2 3)"))
    expect(toJson(ast)).toEqual(["quote", [1, 2, 3]])
  })

  // Adding tests for quote
  it("should parse a quote value", () => {
    const ast = parse(tokenize("'1"))
    expect(toJson(ast)).toEqual(["quote", 1])
  })

  it("should parse nested quote expressions", () => {
    const ast = parse(tokenize("'('foo)"))
    expect(toJson(ast)).toEqual(["quote", [["quote", "foo"]]])
  })

  it("should parse nested quote values", () => {
    const ast = parse(tokenize("''foo"))
    expect(toJson(ast)).toEqual(["quote", ["quote", "foo"]])
  })
})

describe("Basic Evaluation", () => {
  it("should evaluate addition", () => {
    const result = interpret("(+ (+ 1 2) 2)")
    expect(result).toBe(5)
  })

  it("should evaluate subtraction", () => {
    const result = interpret("(- 5 3)")
    expect(result).toBe(2)
  })

  it("should evaluate multiplication", () => {
    const result = interpret("(* 4 2)")
    expect(result).toBe(8)
  })

  it("should evaluate division", () => {
    const result = interpret("(/ 10 2)")
    expect(result).toBe(5)
  })
})

describe("Variable Definition", () => {
  it("should define and retrieve a variable", () => {
    const result = interpret.pipe("(define x 10)").return("x")
    expect(result).toBe(10)
  })

  it("should update an existing variable", () => {
    const result = interpret
      .pipe("(define x 10)")
      .pipe("(define x 20)")
      .return("x")

    expect(result).toBe(20)
  })
})

describe("Function Definition and Application", () => {
  it("should define and call a simple function", () => {
    const result = interpret
      .pipe("(define square (fn (x) (* x x)))")
      .return("(square 4)")

    expect(result).toBe(16)
  })

  it("should define and call a function with multiple arguments", () => {
    const result = interpret
      .pipe("(define add (fn (x y) (+ x y)))")
      .return("(add 2 3)")

    expect(result).toBe(5)
  })
})

describe("Nested Function Calls", () => {
  it("should evaluate nested function calls", () => {
    const result = interpret
      .pipe("(define square (fn (x) (* x x)))")
      .return("(+ (square 3) (square 4))")

    expect(result).toBe(25)
  })

  it("should handle deeply nested calls", () => {
    const result = interpret
      .pipe("(define double (fn (x) (* 2 x)))")
      .return("(double (double (double 2)))")

    expect(result).toBe(16)
  })
})

describe("Error Handling", () => {
  // Undefined variables
  it("should throw an error for undefined variables", () => {
    expect(() => interpret("y")).toThrow(UndefinedVariableError)
  })

  // Invalid syntax cases
  it("should throw an error for invalid syntax due to incomplete define", () => {
    expect(() => interpret("(define x")).toThrow(InvalidExpressionError)
  })

  it("should throw an error for missing parentheses", () => {
    expect(() => interpret("(+ 1 2")).toThrow(InvalidExpressionError)
  })

  // Invalid arguments in expressions
  it("should throw an error when argument is not a string in define", () => {
    expect(() => interpret("(define 123 456)")).toThrow(InvalidArgumentError)
  })

  // Function related errors
  it("should throw an error when function is expected but got something else", () => {
    expect(() => interpret("(123 456)")).toThrow(InvalidFunctionError)
  })

  // List functions errors
  it("should throw an error when `first` or `rest` does not receive a linked list", () => {
    expect(() => interpret("(first 123)")).toThrow(InvalidExpressionError)
    expect(() => interpret("(rest 123)")).toThrow(InvalidExpressionError)
  })

  // Let and let* related errors
  it("should throw an error when `let` or `let*` bindings are not a list", () => {
    expect(() => interpret("(let x 5)")).toThrow(InvalidExpressionError)
  })

  it("should throw an error when `let` or `let*` binding names are not strings", () => {
    expect(() => interpret("(let ((123 456)) 789)")).toThrow(
      InvalidArgumentError,
    )
  })

  // Function parameter errors
  it("should throw an error when `fn` parameters are not a list", () => {
    expect(() => interpret("(fn 123 (+ 1 2))")).toThrow(InvalidExpressionError)
  })

  it("should throw an error when `fn` parameter names are not strings", () => {
    expect(() => interpret("(fn (123) (+ 1 2))")).toThrow(InvalidArgumentError)
  })

  // Quasiquote usage error
  it("should throw an error when `unquote` is used without `quasiquote`", () => {
    expect(() => interpret(",1")).toThrow(UndefinedVariableError)
  })

  // Division by zero error
  it("should throw an error when dividing by zero", () => {
    expect(() => interpret("(/ 10 0)")).toThrow(InvalidArgumentError)
  })
})

describe("Advanced Functionality", () => {
  it("should allow functions to return functions", () => {
    const adder3 = interpret
      .pipe("(define make-adder (fn (x) (fn (y) (+ x y))))")
      .return("(make-adder 3)")

    expect(adder3(new Cons(4))).toBe(7)
  })

  it("should handle closures", () => {
    const result = interpret
      .pipe("(define adder (fn (x) (fn (y) (+ x y))))")
      .return("((adder 5) 2)")

    expect(result).toBe(7)
  })
})

describe("Conditional Expressions", () => {
  it("should evaluate a simple if expression", () => {
    const result = interpret
      .pipe("(define x 5)")
      .return("(if (> x 3) (+ x 1) (- x 1))")

    expect(result).toBe(6)
  })

  it("should evaluate nested if expressions", () => {
    const result = interpret
      .pipe("(define x 5)")
      .return("(if (> x 10) (* x 2) (if (< x 3) (/ x 2) (+ x 1)))")

    expect(result).toBe(6)
  })
})

describe("List Manipulation", () => {
  it("should evaluate a simple list", () => {
    const result = interpret("'(1 2 3 4 5)")
    expect(toJson(result)).toEqual([1, 2, 3, 4, 5])
  })

  it("should evaluate list operations like car", () => {
    const resultCar = interpret("(first (quote (1 2 3)))")
    expect(resultCar).toBe(1)
  })

  it("should evaluate list operations like cdr", () => {
    const result = interpret
      .pipe("(define cdr (fn (lst) (rest lst)))")
      .return("(cdr (quote (1 2 3)))")

    expect(toJson(result)).toEqual([2, 3])
  })
})

describe("Let and Let*", () => {
  it("should evaluate let with independent bindings", () => {
    const result = interpret(`
    (let ((x 2)
          (y 3))
      (* x y))
  `)
    expect(result).toBe(6) // 2 * 3 = 6
  })

  it("should evaluate let with bindings independent of inner scope", () => {
    const result = interpret.pipe(`(define x 5)`).return(`
      (let ((x 2)
            (y (+ x 3)))
        (* x y))
    `)

    expect(result).toBe(16) // 2 * (5 + 3) = 16
  })

  it("should evaluate let* with sequential bindings", () => {
    const result = interpret(`
      (let* ((x 2)
             (y (+ x 3)))
        (* x y))
    `)
    expect(result).toBe(10) // 2 * (2 + 3) = 10
  })

  it("should evaluate let* with dependencies between bindings", () => {
    const result = interpret(`
      (let* ((a 1)
             (b (+ a 2))
             (c (+ b 3)))
        (+ a b c))
    `)
    expect(result).toBe(10) // 1 + (1 + 2) + ((1 + 2) + 3) = 10
  })

  it("should evaluate let* with local shadowing", () => {
    const result = interpret.pipe(`(define x 10) `).return(
      `(let* ((x 2)
             (y (* x 3)))
        (+ x y))`,
    )

    expect(result).toBe(8) // 2 + (2 * 3) = 8
  })

  it("should not affect outer scope with let", () => {
    const result = interpret
      .pipe(`(define z 5)`)
      .pipe(`(let ((z 10)) z)`)
      .return("z")

    expect(result).toBe(5) // The outer z should remain unchanged
  })

  it("should evaluate nested let expressions correctly", () => {
    const result = interpret(`
      (let ((x 2))
        (let ((y 3))
          (+ x y)))
    `)
    expect(result).toBe(5) // 2 + 3 = 5
  })

  it("should handle complex let* with multiple dependent bindings", () => {
    const result = interpret(`
      (let* ((x 2)
             (y (* x 3))
             (z (+ x y)))
        z)
    `)
    expect(result).toBe(8) // 2 + (2 * 3) = 8
  })
})

describe("Quote and Unquote", () => {
  it("should return a list as data when quoted", () => {
    const result = interpret("'(1 2 3)")
    expect(toJson(result)).toEqual([1, 2, 3]) // Should return the list as data, not evaluate it
  })

  it("should return a symbol as data when quoted", () => {
    const result = interpret("'x")
    expect(toJson(result)).toEqual("x") // Should return the symbol 'x' as data
  })

  it("should evaluate an expression with unquote", () => {
    const result = interpret.pipe("(define x 10)").return("`(1 2 ,x 4)")
    expect(toJson(result)).toEqual([1, 2, 10, 4]) // Should evaluate x as 10 in the quoted list
  })

  it("should handle nested quotes correctly", () => {
    const result = interpret("(quote (quote (1 2 3)))")
    expect(toJson(result)).toEqual(["quote", [1, 2, 3]]) // Should return the inner quote as data
  })

  it("should handle unquote in a nested structure", () => {
    const result = interpret.pipe("(define y 5)").return("`(a b (c ,y))")
    expect(toJson(result)).toEqual(["a", "b", ["c", 5]]) // Should evaluate y inside the nested list
  })

  it("should return the list structure as-is when quoted with no unquotes", () => {
    const result = interpret("(quote (a b c d))")
    expect(toJson(result)).toEqual(["a", "b", "c", "d"]) // Should return the list as data
  })

  it("should evaluate multiple unquotes within a single list", () => {
    const result = interpret
      .pipe("(define a 1)")
      .pipe("(define b 2)")
      .return("`(,a ,b ,(+ a b))")
    expect(toJson(result)).toEqual([1, 2, 3]) // a = 1, b = 2, (+ a b) = 3
  })
})
