import { describe, expect, it } from "vitest"
import { evaluate, parse, standardEnvironment, tokenize } from "../src"

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
    expect(ast).toEqual(["+", 1, 2])
  })

  it("should parse nested expressions", () => {
    const ast = parse(tokenize("(* (+ 1 2) (- 3 4))"))
    expect(ast).toEqual(["*", ["+", 1, 2], ["-", 3, 4]])
  })

  it("should parse a define statement", () => {
    const ast = parse(tokenize("(define x 10)"))
    expect(ast).toEqual(["define", "x", 10])
  })

  it("should parse a function definition", () => {
    const ast = parse(tokenize("(define square (fn (x) (* x x)))"))
    expect(ast).toEqual(["define", "square", ["fn", ["x"], ["*", "x", "x"]]])
  })

  // Adding tests for quote
  it("should parse a quote expression", () => {
    const ast = parse(tokenize("'(1 2 3)"))
    expect(ast).toEqual(["quote", [1, 2, 3]])
  })

  // Adding tests for quote
  it("should parse a quote value", () => {
    const ast = parse(tokenize("'1"))
    expect(ast).toEqual(["quote", 1])
  })

  it("should parse nested quote expressions", () => {
    const ast = parse(tokenize("'('foo)"))
    expect(ast).toEqual(["quote", [["quote", "foo"]]])
  })

  it("should parse nested quote values", () => {
    const ast = parse(tokenize("''foo"))
    expect(ast).toEqual(["quote", ["quote", "foo"]])
  })
})

describe("Basic Evaluation", () => {
  it("should evaluate addition", () => {
    const result = evaluate(parse(tokenize("(+ 1 2)")), standardEnvironment())
    expect(result).toBe(3)
  })

  it("should evaluate subtraction", () => {
    const result = evaluate(parse(tokenize("(- 5 3)")), standardEnvironment())
    expect(result).toBe(2)
  })

  it("should evaluate multiplication", () => {
    const result = evaluate(parse(tokenize("(* 4 2)")), standardEnvironment())
    expect(result).toBe(8)
  })

  it("should evaluate division", () => {
    const result = evaluate(parse(tokenize("(/ 10 2)")), standardEnvironment())
    expect(result).toBe(5)
  })
})

describe("Variable Definition", () => {
  it("should define and retrieve a variable", () => {
    const env = standardEnvironment()
    evaluate(parse(tokenize("(define x 10)")), env)
    const result = evaluate(parse(tokenize("x")), env)
    expect(result).toBe(10)
  })

  it("should update an existing variable", () => {
    const env = standardEnvironment()
    evaluate(parse(tokenize("(define x 10)")), env)
    evaluate(parse(tokenize("(define x 20)")), env)
    const result = evaluate(parse(tokenize("x")), env)
    expect(result).toBe(20)
  })
})

describe("Function Definition and Application", () => {
  it("should define and call a simple function", () => {
    const env = standardEnvironment()
    evaluate(parse(tokenize("(define square (fn (x) (* x x)))")), env)
    const result = evaluate(parse(tokenize("(square 4)")), env)
    expect(result).toBe(16)
  })

  it("should define and call a function with multiple arguments", () => {
    const env = standardEnvironment()
    evaluate(parse(tokenize("(define add (fn (x y) (+ x y)))")), env)
    const result = evaluate(parse(tokenize("(add 2 3)")), env)
    expect(result).toBe(5)
  })
})

describe("Nested Function Calls", () => {
  it("should evaluate nested function calls", () => {
    const env = standardEnvironment()
    evaluate(parse(tokenize("(define square (fn (x) (* x x)))")), env)
    const result = evaluate(parse(tokenize("(+ (square 3) (square 4))")), env)
    expect(result).toBe(25)
  })

  it("should handle deeply nested calls", () => {
    const env = standardEnvironment()
    evaluate(parse(tokenize("(define double (fn (x) (* 2 x)))")), env)
    const result = evaluate(
      parse(tokenize("(double (double (double 2)))")),
      env,
    )
    expect(result).toBe(16)
  })
})

describe("Error Handling", () => {
  it("should throw an error for undefined variables", () => {
    const env = standardEnvironment()
    expect(() => evaluate(parse(tokenize("y")), env)).toThrow()
  })

  it("should throw an error for invalid syntax", () => {
    expect(() => parse(tokenize("(define x"))).toThrow(SyntaxError)
  })

  it("should throw an error for missing parentheses", () => {
    expect(() => parse(tokenize("(+ 1 2"))).toThrow(SyntaxError)
  })
})

describe("Advanced Functionality", () => {
  it("should allow functions to return functions", () => {
    const env = standardEnvironment()
    evaluate(
      parse(tokenize("(define make-adder (fn (x) (fn (y) (+ x y))))")),
      env,
    )
    const adder3 = evaluate(parse(tokenize("(make-adder 3)")), env)
    expect(adder3(4)).toBe(7)
  })

  it("should handle closures", () => {
    const env = standardEnvironment()
    evaluate(parse(tokenize("(define adder (fn (x) (fn (y) (+ x y))))")), env)
    const result = evaluate(parse(tokenize("((adder 5) 2)")), env)
    expect(result).toBe(7)
  })
})

describe("Conditional Expressions", () => {
  it("should evaluate a simple if expression", () => {
    const env = standardEnvironment()
    evaluate(parse(tokenize("(define x 5)")), env)
    const tokens = tokenize("(if (> x 3) (+ x 1) (- x 1))")
    const ast = parse(tokens)
    const result = evaluate(ast, env)
    expect(result).toBe(6)
  })

  it("should evaluate nested if expressions", () => {
    const env = standardEnvironment()
    evaluate(parse(tokenize("(define x 5)")), env)
    const result = evaluate(
      parse(tokenize("(if (> x 10) (* x 2) (if (< x 3) (/ x 2) (+ x 1)))")),
      env,
    )
    expect(result).toBe(6)
  })
})

describe("List Manipulation", () => {
  it("should evaluate a simple list", () => {
    const env = standardEnvironment()
    const result = evaluate(parse(tokenize("'(1 2 3 4 5)")), env)
    expect(result).toEqual([1, 2, 3, 4, 5])
  })

  it("should evaluate list operations like car and cdr", () => {
    const env = standardEnvironment()
    evaluate(parse(tokenize("(define car (fn (lst) (first lst)))")), env)
    evaluate(parse(tokenize("(define cdr (fn (lst) (rest lst)))")), env)
    const resultCar = evaluate(parse(tokenize("(car '(1 2 3))")), env)
    const resultCdr = evaluate(parse(tokenize("(cdr '(1 2 3))")), env)
    expect(resultCar).toBe(1)
    expect(resultCdr).toEqual([2, 3])
  })
})

describe("Let and Let*", () => {
  it("should evaluate let with independent bindings", () => {
    const env = standardEnvironment()
    const result = evaluate(
      parse(
        tokenize(`
      (let ((x 2)
            (y 3))
        (* x y))
    `),
      ),
      env,
    )
    expect(result).toBe(6) // 2 * 3 = 6
  })

  it("should evaluate let with bindings independent of inner scope", () => {
    const env = standardEnvironment()
    evaluate(
      parse(
        tokenize(`
      (define x 5)
    `),
      ),
      env,
    )

    const result = evaluate(
      parse(
        tokenize(`
      (let ((x 2)
            (y (+ x 3)))
        (* x y))
    `),
      ),
      env,
    )

    expect(result).toBe(16) // 2 * (5 + 3) = 16
  })

  it("should evaluate let* with sequential bindings", () => {
    const env = standardEnvironment()
    const result = evaluate(
      parse(
        tokenize(`
      (let* ((x 2)
             (y (+ x 3)))
        (* x y))
    `),
      ),
      env,
    )
    expect(result).toBe(10) // 2 * (2 + 3) = 10
  })

  it("should evaluate let* with dependencies between bindings", () => {
    const env = standardEnvironment()
    const result = evaluate(
      parse(
        tokenize(`
      (let* ((a 1)
             (b (+ a 2))
             (c (+ b 3)))
        (+ a b c))
    `),
      ),
      env,
    )
    expect(result).toBe(10) // 1 + (1 + 2) + ((1 + 2) + 3) = 10
  })

  it("should evaluate let* with local shadowing", () => {
    const env = standardEnvironment()
    evaluate(
      parse(
        tokenize(`
      (define x 10)
    `),
      ),
      env,
    )

    const result = evaluate(
      parse(
        tokenize(`
      (let* ((x 2)
             (y (* x 3)))
        (+ x y))
    `),
      ),
      env,
    )

    expect(result).toBe(8) // 2 + (2 * 3) = 8
  })

  it("should not affect outer scope with let", () => {
    const env = standardEnvironment()
    evaluate(
      parse(
        tokenize(`
      (define z 5)
    `),
      ),
      env,
    )

    evaluate(
      parse(
        tokenize(`
      (let ((z 10))
        z)
    `),
      ),
      env,
    )

    const result = evaluate(parse(tokenize(`z`)), env)
    expect(result).toBe(5) // The outer z should remain unchanged
  })

  it("should evaluate nested let expressions correctly", () => {
    const env = standardEnvironment()
    const result = evaluate(
      parse(
        tokenize(`
      (let ((x 2))
        (let ((y 3))
          (+ x y)))
    `),
      ),
      env,
    )
    expect(result).toBe(5) // 2 + 3 = 5
  })

  it("should handle complex let* with multiple dependent bindings", () => {
    const env = standardEnvironment()
    const result = evaluate(
      parse(
        tokenize(`
      (let* ((x 2)
             (y (* x 3))
             (z (+ x y)))
        z)
    `),
      ),
      env,
    )
    expect(result).toBe(8) // 2 + (2 * 3) = 8
  })
})

describe("Quote and Unquote", () => {
  it("should return a list as data when quoted", () => {
    const env = standardEnvironment()
    const ast = parse(tokenize("'(1 2 3)"))
    const result = evaluate(ast, env)
    expect(result).toEqual([1, 2, 3]) // Should return the list as data, not evaluate it
  })

  it("should return a symbol as data when quoted", () => {
    const env = standardEnvironment()
    const ast = parse(tokenize("'x"))
    const result = evaluate(ast, env)
    expect(result).toEqual("x") // Should return the symbol 'x' as data
  })

  it("should evaluate an expression with unquote", () => {
    const env = standardEnvironment()
    evaluate(parse(tokenize("(define x 10)")), env)
    const ast = parse(tokenize("`(1 2 ,x 4)"))
    const result = evaluate(ast, env)
    expect(result).toEqual([1, 2, 10, 4]) // Should evaluate x as 10 in the quoted list
  })

  it("should handle nested quotes correctly", () => {
    const env = standardEnvironment()
    const ast = parse(tokenize("(quote (quote (1 2 3)))"))
    const result = evaluate(ast, env)
    expect(result).toEqual(["quote", [1, 2, 3]]) // Should return the inner quote as data
  })

  it("should handle unquote in a nested structure", () => {
    const env = standardEnvironment()
    evaluate(parse(tokenize("(define y 5)")), env)
    const result = evaluate(parse(tokenize("`(a b (c ,y))")), env)
    expect(result).toEqual(["a", "b", ["c", 5]]) // Should evaluate y inside the nested list
  })

  it("should return the list structure as-is when quoted with no unquotes", () => {
    const env = standardEnvironment()
    const result = evaluate(parse(tokenize("(quote (a b c d))")), env)
    expect(result).toEqual(["a", "b", "c", "d"]) // Should return the list as data
  })

  it("should evaluate multiple unquotes within a single list", () => {
    const env = standardEnvironment()
    evaluate(parse(tokenize("(define a 1)")), env)
    evaluate(parse(tokenize("(define b 2)")), env)
    const result = evaluate(parse(tokenize("`(,a ,b ,(+ a b))")), env)
    expect(result).toEqual([1, 2, 3]) // a = 1, b = 2, (+ a b) = 3
  })
})
