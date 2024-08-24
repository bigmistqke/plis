import { Cons, destructure, forEach, isCons, map, reduce } from "./cons"
import { toJson } from "./utils"
export * from "./cons"
export * from "./utils"

const DEBUG = false

/**********************************************************************************/
/*                                                                                */
/*                                      Types                                     */
/*                                                                                */
/**********************************************************************************/

export type AstValue = string | number | null
export type Ast = Cons<AstValue | Ast, Ast>
export type AstJson = AstValue | Array<AstJson>
export type Mode = "evaluate" | "quote" | "quasiquote"

/**********************************************************************************/
/*                                                                                */
/*                                     Errors                                     */
/*                                                                                */
/**********************************************************************************/
export class UndefinedVariableError extends Error {
  constructor(variable: string) {
    super(`Undefined variable: ${variable}`)
    this.name = "UndefinedVariableError"
  }
}

export class InvalidExpressionError extends Error {
  constructor(message: string) {
    super(message)
    this.name = "InvalidExpressionError"
  }
}

export class InvalidArgumentError extends Error {
  constructor(message: string) {
    super(message)
    this.name = "InvalidArgumentError"
  }
}

export class InvalidFunctionError extends Error {
  constructor(proc: any, expression: any) {
    super(
      `Function expected, but got ${typeof proc}: ${JSON.stringify(
        expression,
      )}`,
    )
    this.name = "InvalidFunctionError"
  }
}

/**********************************************************************************/
/*                                                                                */
/*                              Standard Environment                              */
/*                                                                                */
/**********************************************************************************/

class Environment {
  constructor(
    public parent: Environment | null,
    public values: Record<string, any> = {},
  ) {}
  get(symbol: string): any | undefined {
    if (symbol in this.values) {
      return this.values[symbol]
    }
    return this.parent?.get(symbol)
  }
  set(symbol: string, value: any) {
    this.values[symbol] = value
  }
}

export function createStandardEnvironment(): Environment {
  return new Environment(null, {
    "+": (_args: Cons) =>
      reduce(_args, (a, _b) => (a === undefined ? _b.value : a + _b.value)),
    "-": (_args: Cons) =>
      reduce(_args, (a, _b) => (a === undefined ? _b.value : a - _b.value)),
    "*": (args: Cons) =>
      reduce(args, (a, _b) => (a === undefined ? _b.value : a * _b.value)),
    "/": (_args: Cons) =>
      reduce(_args, (a, _b) => (a === undefined ? _b.value : a / _b.value)),
    ">": (_args: Cons) => {
      const [_a, _b] = destructure(_args)
      return _a.value > _b.value
    },
    "<": (_args: Cons) => {
      const [_a, _b] = destructure(_args)
      return _a.value < _b.value
    },
    True: true,
    False: false,
    first: (_args: Cons) => {
      const list = _args.value
      if (!isCons(list)) {
        throw new InvalidExpressionError(
          "fn: first expects first argument to be a linked list",
        )
      }
      return list.value
    },
    rest: (_args: Cons) => {
      const list = _args.value
      if (!isCons(list)) {
        throw new InvalidExpressionError(
          "fn: rest expects first argument to be a linked list",
        )
      }
      const [_first] = destructure(list, 1)
      return _first.next
    },
  })
}

/**********************************************************************************/
/*                                                                                */
/*                                    Tokenize                                    */
/*                                                                                */
/**********************************************************************************/

// NOTE: Current tokenizer does not handle comments or strings properly
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

/**********************************************************************************/
/*                                                                                */
/*                                      Parse                                     */
/*                                                                                */
/**********************************************************************************/

export function parse(tokens: string[]): Ast | AstValue | null {
  if (tokens.length === 0) {
    throw new InvalidExpressionError("Unexpected EOF")
  }

  const token = tokens.shift()!
  switch (token) {
    case "(":
      let current: Ast | null = null
      let head: Ast | null = null
      while (tokens[0] !== ")") {
        const result = parse(tokens)
        if (current === null) {
          head = current = new Cons(result)
        } else {
          current.next = new Cons(result)
          current = current.next
        }
      }
      tokens.shift()
      return head
    case "'":
      return new Cons("quote", new Cons(parse(tokens)))
    case ",":
      return new Cons("unquote", new Cons(parse(tokens)))
    case "`":
      return new Cons("quasiquote", new Cons(parse(tokens)))
    default:
      return atom(token)
  }
}

function atom(token: string) {
  return isNaN(Number(token)) ? token : Number(token)
}

/**********************************************************************************/
/*                                                                                */
/*                                      Scope                                     */
/*                                                                                */
/**********************************************************************************/

class Scope {
  array = new Array<Mode>()
  constructor(...values: Mode[]) {
    this.array.push(...values)
  }
  get current() {
    return this.array[this.array.length - 1]
  }
  scope<TValue>(mode: Mode, callback: () => TValue): TValue {
    this.array.push(mode)
    const result = callback()
    this.array.pop()
    return result
  }
}

/**********************************************************************************/
/*                                                                                */
/*                                    Evaluate                                    */
/*                                                                                */
/**********************************************************************************/

export function evaluate(
  expression: Ast | AstValue,
  environment = createStandardEnvironment(),
): any {
  const mode = new Scope("evaluate")

  function evaluate(expression: Ast | AstValue, environment: Environment): any {
    DEBUG && console.info("_evaluate", toJson(expression))
    switch (mode.current) {
      case "evaluate": {
        if (expression === null) {
          return null
        }

        if (typeof expression === "string") {
          if (expression[0] === ",") {
            return evaluate(expression.slice(1, -1), environment)
          }
          const variable = environment.get(expression)
          if (!variable) {
            throw new UndefinedVariableError(expression)
          }
          return variable
        }

        if (typeof expression === "number") {
          return expression
        }

        const [_keyword, _rest] = destructure(expression, 2)
        const keyword = _keyword.value

        switch (keyword) {
          case "define": {
            const [_name, _body] = destructure(_rest, 2)
            if (typeof _name.value !== "string") {
              throw new InvalidArgumentError(
                "Name in 'define' must be a string.",
              )
            }
            environment.set(_name.value, evaluate(_body.value, environment))
            return
          }
          case "fn": {
            const [_params, _body] = destructure(_rest, 2)
            const params = _params.value
            if (!isCons(params)) {
              throw new InvalidExpressionError(
                "Parameters for 'fn' must be a list.",
              )
            }
            return (args: Cons) => {
              const destructedArgs = destructure(args)
              const fnEnvironment: Environment = new Environment(environment)
              forEach(params, (_param, index) => {
                if (typeof _param.value !== "string") {
                  throw new InvalidArgumentError(
                    "Parameter names must be strings.",
                  )
                }
                fnEnvironment.set(_param.value, destructedArgs[index].value)
              })
              return evaluate(_body.value, fnEnvironment)
            }
          }
          case "if": {
            const [_conditional, _success, _fallthrough] = destructure(_rest, 3)
            if (evaluate(_conditional.value, environment)) {
              return evaluate(_success.value, environment)
            } else {
              return evaluate(_fallthrough.value, environment)
            }
          }
          case "let":
          case "let*": {
            const [_params, _body] = destructure(_rest, 2)
            if (!isCons(_params.value)) {
              throw new InvalidExpressionError(
                "Bindings for 'let' or 'let*' must be a list.",
              )
            }
            const letEnvironment = new Environment(environment)
            forEach(_params.value, (_param) => {
              if (!isCons(_param.value)) {
                throw new InvalidExpressionError("Each binding must be a list.")
              }
              const [_name, _value] = destructure(_param.value)
              if (typeof _name.value !== "string") {
                throw new InvalidArgumentError("Binding names must be strings.")
              }
              letEnvironment.set(
                _name.value,
                evaluate(
                  _value.value,
                  keyword === "let" ? environment : letEnvironment,
                ),
              )
            })
            return evaluate(_body.value, letEnvironment)
          }
          case "quote":
            return _rest.value
          case "quasiquote":
            return mode.scope(keyword, () => evaluate(_rest.value, environment))
          default: {
            const proc = evaluate(keyword, environment)
            const args = map(_rest, (_arg) => evaluate(_arg.value, environment))
            if (typeof proc !== "function") {
              throw new InvalidFunctionError(proc, expression)
            }
            return proc(args)
          }
        }
      }
      case "quasiquote": {
        if (isCons(expression)) {
          const [_keyword, _body] = destructure(expression)
          if (_keyword.value === "unquote") {
            return mode.scope("evaluate", () => {
              return evaluate(_body.value, environment)
            })
          }
          return map(expression, (_value) => {
            return evaluate(_value.value, environment)
          })
        }
        return expression
      }
    }
  }

  return evaluate(expression, environment)
}

/**********************************************************************************/
/*                                                                                */
/*                                    Transpile                                   */
/*                                                                                */
/**********************************************************************************/

export function transpile(
  source: string,
  environment = createStandardEnvironment(),
) {
  const ast = parse(tokenize(source))
  return evaluate(ast, environment)
}

transpile.pipe = function (
  source: string,
  environment = createStandardEnvironment(),
) {
  const value = transpile(source, environment)
  return {
    value,
    pipe: (expression: string) => transpile.pipe(expression, environment),
  }
}
