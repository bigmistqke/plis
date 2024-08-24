type TupleOf<T, N extends number, R extends T[] = []> = R["length"] extends N
  ? R
  : TupleOf<T, N, [T, ...R]>

/** A generic, singly linked list node class that holds a value and a reference to the next node. */
export class Cons<TValue = any, TNext extends Cons = any> {
  constructor(
    /** The value stored in the node. */
    public value: TValue,
    /** A reference to the next node in the linked list (defaults to null). */
    public next: TNext | null = null,
  ) {}
}

/** Iterates over each element in a singly linked list, applying a callback function. */
export function forEach<TValue, TNext extends Cons>(
  /** The head of the singly linked list. */
  cons: Cons<TValue, TNext> | null,
  /** The callback function to apply to each node. */
  callback: (cons: Cons<TValue>, index: number) => void,
  /** The maximum number of nodes to iterate through (defaults to Infinity). */
  amount = Infinity,
) {
  let current: Cons<TValue, TNext> | null = cons
  let index = 0
  while (current && index < amount) {
    callback(current, index)
    index++
    current = current instanceof Cons ? current.next : null
  }
}

/** Destructures a singly linked list into an array or a fixed-length tuple based on the specified `amount`. */
export function destructure<TValue, TNext extends Cons, TAmount extends number>(
  /** The head of the singly linked list. */
  cons: Cons<TValue, TNext> | null,
  /** The number of elements to return. */
  amount: TAmount,
): TupleOf<Cons<TValue>, TAmount>
export function destructure<TValue, TNext extends Cons>(
  /** The head of the singly linked list. */
  cons: Cons<TValue, TNext> | null,
): Cons<TValue>[]
export function destructure(cons: Cons | null, amount?: number) {
  const conses: Cons[] = []
  forEach(cons, (value) => conses.push(value), amount)
  return conses
}

/** Maps each node in a singly linked list to a new value determined by a callback function. */
export function map<TValue, TNext extends Cons, V extends Cons, TReturn>(
  /** The head of the singly linked list. */
  cons: Cons<TValue, TNext>,
  /** The callback function that generates a new value. */
  callback: (value: Cons, index: number) => TReturn,
): Cons<TReturn, V> {
  try {
    let head = new Cons(callback(cons, 0))
    let current = head
    forEach(cons.next, (value, index) => {
      current.next = new Cons(callback(value, index + 1))
      current = current.next
    })
    return head
  } catch (err) {
    console.error("error while mapping", err, cons)
    throw err
  }
}

/** Reduces the elements of a singly linked list into a single value based on a callback function. */
export function reduce<TValue>(
  /** The head of the singly linked list. */
  cons: Cons<TValue>,
  /** The callback function that processes each node. */
  callback: (
    aggregator: TValue | undefined,
    current: Cons<TValue>,
    index: number,
  ) => TValue,
): TValue
export function reduce<TValue, TAggregator>(
  /** The head of the singly linked list. */
  cons: Cons<TValue>,
  /** The callback function that processes each node. */
  callback: (
    aggregator: TAggregator | undefined,
    current: TValue,
    index: number,
  ) => TAggregator,
  /** Initial value to start the reduction. */
  initialValue?: TAggregator,
): TAggregator
export function reduce(
  cons: Cons,
  callback: (aggregator: any, current: any, index: number) => any,
  initialValue?: any,
) {
  let aggregator: any | undefined = initialValue
  forEach(cons, (current, index) => {
    aggregator = callback(aggregator, current, index)
  })
  return aggregator
}

export function isCons(value: any): value is Cons {
  return value instanceof Cons
}
