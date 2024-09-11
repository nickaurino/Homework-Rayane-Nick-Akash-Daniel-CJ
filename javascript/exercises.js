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
export function say(word) {
  // store words in internal array, init empty if none available
  say.words = say.words || []

  // make sure word exists
  if (word !== undefined) {
    say.words.push(word)
    return say
  }

  // handle no arguments
  const result = say.words.join(" ")

  // reset function state
  say.words = []

  return result
}

// Write your line count function here
export async function meaningfulLineCount(fileName) {
  try {
    const fileHandle = await open(fileName, "r")

    const content = await fileHandle.readFile("utf-8")

    await fileHandle.close()

    const lines = content.split("\n")

    const validLines = lines.filter((line) => {
      const trimmedLine = line.trim()
      return trimmedLine && !trimmedLine.startsWith("#")
    })

    return validLines.length
  } catch (error) {
    throw new Error(`Error reading file: ${error.message}`)
  }
}

// Write your Quaternion class here
