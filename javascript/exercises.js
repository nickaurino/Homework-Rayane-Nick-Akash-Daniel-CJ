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

export class Quaternion {
  constructor(a, b, c, d) {
    this.a = a; 
    this.b = b; 
    this.c = c; 
    this.d = d; 


    Object.freeze(this);
  }


  get coefficients() {
    return [this.a, this.b, this.c, this.d];
  }


  get conjugate() {
    return new Quaternion(this.a, -this.b, -this.c, -this.d);
  }


  plus(quaternion) {
    return new Quaternion(
      this.a + quaternion.a,
      this.b + quaternion.b,
      this.c + quaternion.c,
      this.d + quaternion.d
    );
  }


  times(quaternion) {
    const a1 = this.a, b1 = this.b, c1 = this.c, d1 = this.d;
    const a2 = quaternion.a, b2 = quaternion.b, c2 = quaternion.c, d2 = quaternion.d;
    
    return new Quaternion(
      a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2,
      a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2,
      a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2,
      a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2
    );
  }
  toString() {
    const parts = [];
    if (this.a !== 0) parts.push(this.a);
    if (this.b !== 0) parts.push(`${this.b === 1 ? '' : this.b === -1 ? '-' : this.b}i`);
    if (this.c !== 0) parts.push(`${this.c === 1 ? '' : this.c === -1 ? '-' : this.c}j`);
    if (this.d !== 0) parts.push(`${this.d === 1 ? '' : this.d === -1 ? '-' : this.d}k`);
    

    return parts.join('+').replace(/\+-/g, '-').replace(/^\+/, '') || '0';
  }
}


