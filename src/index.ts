// TODO:  replace with tokenizer that properly handles strings and comments
export function tokenize(expression: string): string[] {
  return expression
    .replace(/\(/g, " ( ")
    .replace(/\)/g, " ) ")
    .replace(/\,/g, " , ")
    .trim()
    .split(/\s+/)
}

export function parse(tokens: string[]) {
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

export function evaluate(
  expression: any,
  environment = standardEnvironment(),
): any {
  if (typeof expression === "string") {
    if (expression[0] === ",") {
      return evaluate(expression.slice(1, -1), environment)
    }
    if (!(expression in environment)) {
      console.error("Variable is undefined", expression)
      throw `Variable ${expression} is undefined.`
    }
    return environment[expression]
  } else if (typeof expression === "number") {
    return expression
  } else {
    switch (expression[0]) {
      case "quote": {
        const [, list] = expression
        return list
      }
      case "define": {
        const [, varName, expr] = expression
        environment[varName] = evaluate(expr, environment)
        return
      }
      case "let":
      case "let*": {
        const [type, params, body] = expression
        const scopedEnvironment = {
          ...environment,
        }
        console.log("params", params)
        params.forEach((param) => {
          const [name, value] = param
          scopedEnvironment[name] = evaluate(
            value,
            type === "let" ? environment : scopedEnvironment,
          )
        })
        return evaluate(body, scopedEnvironment)
      }
      case "if": {
        const [, conditional, success, fallthrough] = expression
        if (evaluate(conditional, environment)) {
          return evaluate(success, environment)
        } else {
          return evaluate(fallthrough, environment)
        }
      }
      case "fn": {
        const [, params, body] = expression
        return (...args: any[]) => {
          const localEnv: Environment = { ...environment }
          params.forEach((param: string, index: number) => {
            localEnv[param] = args[index]
          })
          return evaluate(body, localEnv)
        }
      }

      default: {
        const [fn, ...args] = expression
        const proc = evaluate(fn, environment)
        const evaluatedArgs = args.map((arg) => evaluate(arg, environment))
        if (typeof proc !== "function") {
          console.error(`proc is not a function`, proc, expression)
          return
        }
        console.log("fn", fn, proc, args, evaluatedArgs)
        return proc(...evaluatedArgs)
      }
    }
  }
}
