import { describe, expect, it } from "vitest"
import {
  Cons,
  InvalidExpressionError,
  parse,
  toJson,
  tokenize,
  transpile,
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
    const result = transpile("(+ (+ 1 2) 2)")
    expect(result).toBe(5)
  })

  it("should evaluate subtraction", () => {
    const result = transpile("(- 5 3)")
    expect(result).toBe(2)
  })

  it("should evaluate multiplication", () => {
    const result = transpile("(* 4 2)")
    expect(result).toBe(8)
  })

  it("should evaluate division", () => {
    const result = transpile("(/ 10 2)")
    expect(result).toBe(5)
  })
})

describe("Variable Definition", () => {
  it("should define and retrieve a variable", () => {
    const result = transpile.pipe("(define x 10)").pipe("x").value
    expect(result).toBe(10)
  })

  it("should update an existing variable", () => {
    const result = transpile
      .pipe("(define x 10)")
      .pipe("(define x 20)")
      .pipe("x").value

    expect(result).toBe(20)
  })
})

describe("Function Definition and Application", () => {
  it("should define and call a simple function", () => {
    const result = transpile
      .pipe("(define square (fn (x) (* x x)))")
      .pipe("(square 4)").value

    expect(result).toBe(16)
  })

  it("should define and call a function with multiple arguments", () => {
    const result = transpile
      .pipe("(define add (fn (x y) (+ x y)))")
      .pipe("(add 2 3)").value

    expect(result).toBe(5)
  })
})

describe("Nested Function Calls", () => {
  it("should evaluate nested function calls", () => {
    const result = transpile
      .pipe("(define square (fn (x) (* x x)))")
      .pipe("(+ (square 3) (square 4))").value

    expect(result).toBe(25)
  })

  it("should handle deeply nested calls", () => {
    const result = transpile
      .pipe("(define double (fn (x) (* 2 x)))")
      .pipe("(double (double (double 2)))").value

    expect(result).toBe(16)
  })
})

describe("Error Handling", () => {
  it("should throw an error for undefined variables", () => {
    expect(() => transpile("y")).toThrow()
  })

  it("should throw an error for invalid syntax", () => {
    expect(() => parse(tokenize("(define x"))).toThrow(InvalidExpressionError)
  })

  it("should throw an error for missing parentheses", () => {
    expect(() => parse(tokenize("(+ 1 2"))).toThrow(InvalidExpressionError)
  })
})

describe("Advanced Functionality", () => {
  it("should allow functions to return functions", () => {
    const adder3 = transpile
      .pipe("(define make-adder (fn (x) (fn (y) (+ x y))))")
      .pipe("(make-adder 3)").value

    expect(adder3(new Cons(4))).toBe(7)
  })

  it("should handle closures", () => {
    const result = transpile
      .pipe("(define adder (fn (x) (fn (y) (+ x y))))")
      .pipe("((adder 5) 2)").value

    expect(result).toBe(7)
  })
})

describe("Conditional Expressions", () => {
  it("should evaluate a simple if expression", () => {
    const result = transpile
      .pipe("(define x 5)")
      .pipe("(if (> x 3) (+ x 1) (- x 1))").value

    expect(result).toBe(6)
  })

  it("should evaluate nested if expressions", () => {
    const result = transpile
      .pipe("(define x 5)")
      .pipe("(if (> x 10) (* x 2) (if (< x 3) (/ x 2) (+ x 1)))").value

    expect(result).toBe(6)
  })
})

describe("List Manipulation", () => {
  it("should evaluate a simple list", () => {
    const result = transpile("'(1 2 3 4 5)")
    expect(toJson(result)).toEqual([1, 2, 3, 4, 5])
  })

  it("should evaluate list operations like car", () => {
    const resultCar = transpile("(first (quote (1 2 3)))")
    expect(resultCar).toBe(1)
  })

  it("should evaluate list operations like cdr", () => {
    const result = transpile
      .pipe("(define cdr (fn (lst) (rest lst)))")
      .pipe("(cdr (quote (1 2 3)))").value

    expect(toJson(result)).toEqual([2, 3])
  })
})

describe("Let and Let*", () => {
  it("should evaluate let with independent bindings", () => {
    const result = transpile(`
    (let ((x 2)
          (y 3))
      (* x y))
  `)
    expect(result).toBe(6) // 2 * 3 = 6
  })

  it("should evaluate let with bindings independent of inner scope", () => {
    const result = transpile.pipe(`(define x 5)`).pipe(`
      (let ((x 2)
            (y (+ x 3)))
        (* x y))
    `).value

    expect(result).toBe(16) // 2 * (5 + 3) = 16
  })

  it("should evaluate let* with sequential bindings", () => {
    const result = transpile(`
      (let* ((x 2)
             (y (+ x 3)))
        (* x y))
    `)
    expect(result).toBe(10) // 2 * (2 + 3) = 10
  })

  it("should evaluate let* with dependencies between bindings", () => {
    const result = transpile(`
      (let* ((a 1)
             (b (+ a 2))
             (c (+ b 3)))
        (+ a b c))
    `)
    expect(result).toBe(10) // 1 + (1 + 2) + ((1 + 2) + 3) = 10
  })

  it("should evaluate let* with local shadowing", () => {
    const result = transpile.pipe(`(define x 10) `).pipe(
      `(let* ((x 2)
             (y (* x 3)))
        (+ x y))`,
    ).value

    expect(result).toBe(8) // 2 + (2 * 3) = 8
  })

  it("should not affect outer scope with let", () => {
    const result = transpile
      .pipe(`(define z 5)`)
      .pipe(`(let ((z 10)) z)`)
      .pipe("z").value

    expect(result).toBe(5) // The outer z should remain unchanged
  })

  it("should evaluate nested let expressions correctly", () => {
    const result = transpile(`
      (let ((x 2))
        (let ((y 3))
          (+ x y)))
    `)
    expect(result).toBe(5) // 2 + 3 = 5
  })

  it("should handle complex let* with multiple dependent bindings", () => {
    const result = transpile(`
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
    const result = transpile("'(1 2 3)")
    expect(toJson(result)).toEqual([1, 2, 3]) // Should return the list as data, not evaluate it
  })

  it("should return a symbol as data when quoted", () => {
    const result = transpile("'x")
    expect(toJson(result)).toEqual("x") // Should return the symbol 'x' as data
  })

  it("should evaluate an expression with unquote", () => {
    const result = transpile.pipe("(define x 10)").pipe("`(1 2 ,x 4)").value
    expect(toJson(result)).toEqual([1, 2, 10, 4]) // Should evaluate x as 10 in the quoted list
  })

  it("should handle nested quotes correctly", () => {
    const result = transpile("(quote (quote (1 2 3)))")
    expect(toJson(result)).toEqual(["quote", [1, 2, 3]]) // Should return the inner quote as data
  })

  it("should handle unquote in a nested structure", () => {
    const result = transpile.pipe("(define y 5)").pipe("`(a b (c ,y))").value
    expect(toJson(result)).toEqual(["a", "b", ["c", 5]]) // Should evaluate y inside the nested list
  })

  it("should return the list structure as-is when quoted with no unquotes", () => {
    const result = transpile("(quote (a b c d))")
    expect(toJson(result)).toEqual(["a", "b", "c", "d"]) // Should return the list as data
  })

  it("should evaluate multiple unquotes within a single list", () => {
    const result = transpile
      .pipe("(define a 1)")
      .pipe("(define b 2)")
      .pipe("`(,a ,b ,(+ a b))").value
    expect(toJson(result)).toEqual([1, 2, 3]) // a = 1, b = 2, (+ a b) = 3
  })
})
