import { Ast, AstJson, AstValue } from "."
import { Cons, forEach } from "./cons"

export function toJson(value: Ast | AstValue | null): AstJson {
  if (value === null) return null
  if (value instanceof Cons) {
    const values: AstJson = []
    forEach(value, (value) => {
      if (value.value instanceof Cons) {
        values.push(toJson(value.value))
      } else {
        values.push(value.value)
      }
    })
    return values
  }
  return value
}
