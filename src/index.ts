// TODO:  replace with tokenizer that properly handles strings and comments
export function tokenize(expression: string): string[] {
  return expression
    .replace(/\(/g, " ( ")
    .replace(/\)/g, " ) ")
    .replace(/\,/g, " , ")
    .replace(/\'/g, " ' ")
    .replace(/\`/g, " ` ")
    .trim()
    .split(/\s+/)
}

export function parse(tokens: string[]): any {
  if (tokens.length === 0) {
    throw new SyntaxError("Unexpected EOF")
  }

  const token = tokens.shift()!

  switch (token) {
    case "(":
      const expression = []
      while (tokens[0] !== ")") {
        expression.push(parse(tokens))
      }
      tokens.shift() // Discard ')'
      return expression
    case "'":
      return ["quote", parse(tokens)]
    case ",":
      return ["unquote", parse(tokens)]
    case "`":
      return ["quasiquote", parse(tokens)]
    default:
      return atom(token)
  }
}

function atom(token: string) {
  return isNaN(Number(token)) ? token : Number(token)
}

type Environment = Record<string, any>

export function standardEnvironment(): Environment {
  return {
    "+": (...values: number[]) => values.reduce((a, b) => a + b),
    "-": (...values: number[]) => values.reduce((a, b) => a - b),
    "*": (...values: number[]) => values.reduce((a, b) => a * b),
    "/": (...values: number[]) => values.reduce((a, b) => a / b),
    ">": (a: number, b: number) => a > b,
    "<": (a: number, b: number) => a < b,
    True: true,
    False: false,
    first: (lst: any[]) => lst[0],
    rest: (lst: any[]) => lst.slice(1),
  }
}

type Mode = "evaluate" | "quote" | "quasiquote"
class ScopeStack<T> {
  array = new Array<T>()
  constructor(...values: T[]) {
    this.array.push(...values)
  }
  get current() {
    return this.array[this.array.length - 1]
  }
  scope<TValue>(mode: T, callback: () => TValue): TValue {
    this.array.push(mode)
    const result = callback()
    this.array.pop()
    return result
  }
}

export function evaluate(
  expression: any,
  environment = standardEnvironment(),
): any {
  const mode = new ScopeStack<Mode>("evaluate")
  function _evaluate(
    expression: any,
    environment = standardEnvironment(),
  ): any {
    switch (mode.current) {
      case "evaluate": {
        if (typeof expression === "string") {
          if (expression[0] === ",") {
            return _evaluate(expression.slice(1, -1), environment)
          }
          if (!(expression in environment)) {
            console.error("Variable is undefined", expression)
            throw `Variable ${expression} is undefined.`
          }
          return environment[expression]
        }
        if (typeof expression === "number") {
          return expression
        }
        const [keyword, ...rest] = expression
        switch (keyword) {
          case "define": {
            const [varName, expr] = rest
            environment[varName] = _evaluate(expr, environment)
            return
          }
          case "fn": {
            const [params, body] = rest
            return (...args: any[]) => {
              const localEnv: Environment = { ...environment }
              params.forEach((param: string, index: number) => {
                localEnv[param] = args[index]
              })
              return _evaluate(body, localEnv)
            }
          }
          case "if": {
            const [conditional, success, fallthrough] = rest
            if (_evaluate(conditional, environment)) {
              return _evaluate(success, environment)
            } else {
              return _evaluate(fallthrough, environment)
            }
          }
          case "let":
          case "let*": {
            const [params, body] = rest
            const scopedEnvironment = {
              ...environment,
            }
            params.forEach((param) => {
              const [name, value] = param
              scopedEnvironment[name] = _evaluate(
                value,
                keyword === "let" ? environment : scopedEnvironment,
              )
            })
            return _evaluate(body, scopedEnvironment)
          }
          case "unquote":
          case "quasiquote":
          case "quote": {
            const [value] = rest
            return mode.scope(keyword, () => _evaluate(value, environment))
          }
          default: {
            const [...args] = rest
            const proc = _evaluate(keyword, environment)
            const evaluatedArgs = args.map((arg) => _evaluate(arg, environment))
            if (typeof proc !== "function") {
              console.error(`proc is not a function`, proc, expression)
              return
            }
            return proc(...evaluatedArgs)
          }
        }
      }
      case "quasiquote": {
        if (Array.isArray(expression)) {
          return expression.map((item) => {
            if (Array.isArray(item) && item[0] === "unquote") {
              return mode.scope("evaluate", () =>
                _evaluate(item[1], environment),
              )
            } else {
              return _evaluate(item, environment) // Recursively handle nested lists
            }
          })
        } else {
          return expression
        }
      }
      case "quote": {
        return expression
      }
    }
  }
  return _evaluate(expression, environment)
}
