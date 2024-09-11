import { open } from "node:fs/promises"

export function change(amount) {
  if (!Number.isInteger(amount)) {
    throw new TypeError("Amount must be an integer")
  }
  if (amount < 0) {
    throw new RangeError("Amount cannot be negative")
  }
  let [counts, remaining] = [{}, amount]
  for (const denomination of [25, 10, 5, 1]) {
    counts[denomination] = Math.floor(remaining / denomination)
    remaining %= denomination
  }
  return counts
}

// Write your first then lower case function here
export function firstThenLowerCase(arr, predicate) {
  const element = arr.find(predicate)

  // only if exists, if not return undefined
  return element?.toLowerCase()
}

// Write your powers generator here
export function* powersGenerator({ ofBase, upTo }) {
  let power = 0
  let value = Math.pow(ofBase, power)

  // yield until limit reached
  while (value <= upTo) {
    yield value
    power += 1
    value = Math.pow(ofBase, power)
  }
}

// Write your say function here

// Write your line count function here

// Write your Quaternion class here
