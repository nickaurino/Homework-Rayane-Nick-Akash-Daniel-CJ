import { open } from "node:fs/promises"

export function change(amount: bigint): Map<bigint, bigint> {
  if (amount < 0) {
    throw new RangeError("Amount cannot be negative")
  }
  let counts: Map<bigint, bigint> = new Map()
  let remaining = amount
  for (const denomination of [25n, 10n, 5n, 1n]) {
    counts.set(denomination, remaining / denomination)
    remaining %= denomination
  }
  return counts
}

// Write your first then apply function here
export function firstThenApply<T, U>(
  arr: T[],                        
  predicate: (element: T) => boolean, // Function that tests each element
  fn: (element: T) => U               // Function to apply to the matching element
): U | undefined {                   // Returns the result of fn if a match is found
  
  // Find the first element in arr that satisfies the predicate
  const found = arr.find(predicate);

  // If found is not undefined (a match was found), apply fn to it and return the result
  // If no match was found (found is undefined) and return undefined
  return found !== undefined ? fn(found) : undefined;
}

// Write your powers generator here
export function* powersGenerator(base: bigint): Generator<bigint, void, undefined> {
  let current = 1n; // Start with the base raised to the power of 0, which is 1
  
  while (true) {
    yield current;    // Yield the current power of the base
    current *= base;  // Update current to the next power by multiplying with the base
  }
}

// Write your line count function here
export async function meaningfulLineCount(filePath: string): Promise<number> {
  const file = await open(filePath, "r"); // Open the file in read mode
  let count = 0; // Initialize the count of meaningful lines

  for await (const line of file.readLines()) {
    // Increment count if line is not empty and does not start with '#'
    if (line.trim() && !line.trim().startsWith("#")) {
      count++;
    }
  }

  await file.close(); // Close the file after reading
  return count; // Return the total count of meaningful lines
}


// Write your shape type and associated functions here
export type Shape =
  | { kind: "Box"; width: number; length: number; depth: number } // Represents a box shape with dimensions
  | { kind: "Sphere"; radius: number };                           // Represents a sphere shape with a radius

export function volume(shape: Shape): number {
  if (shape.kind === "Box") {
    // Calculate volume for a box
    return shape.width * shape.length * shape.depth;
  } else {
    // Calculate volume for a sphere
    return (4 / 3) * Math.PI * Math.pow(shape.radius, 3);
  }
}

export function surfaceArea(shape: Shape): number {
  if (shape.kind === "Box") {
    // Calculate surface area for a box
    return 2 * (shape.width * shape.length + shape.length * shape.depth + shape.width * shape.depth);
  } else {
    // Calculate surface area for a sphere
    return 4 * Math.PI * Math.pow(shape.radius, 2);
  }
}

// Write binary search tree implementation 
type TreeNode<T> = {
  value: T;
  left: BinarySearchTree<T>;
  right: BinarySearchTree<T>;
};

export class BinarySearchTree<T> {
  private root: TreeNode<T> | null;

  constructor(root: TreeNode<T> | null = null) {
    this.root = root;
  }

  // Insert a value, returning a new tree with the value added
  insert(value: T): BinarySearchTree<T> {
    if (this.root === null) {
      return new BinarySearchTree({ value, left: new Empty(), right: new Empty() });
    }
    if (value < this.root.value) {
      return new BinarySearchTree({
        value: this.root.value,
        left: this.root.left.insert(value),
        right: this.root.right,
      });
    } else if (value > this.root.value) {
      return new BinarySearchTree({
        value: this.root.value,
        left: this.root.left,
        right: this.root.right.insert(value),
      });
    } else {
      return this; // Value already exists
    }
  }

  // Check if a value exists in the tree
  contains(value: T): boolean {
    if (this.root === null) {
      return false;
    }
    return value === this.root.value
      ? true
      : value < this.root.value
      ? this.root.left.contains(value)
      : this.root.right.contains(value);
  }

  // Calculate the number of nodes in the tree
  size(): number {
    if (this.root === null) {
      return 0;
    }
    return 1 + this.root.left.size() + this.root.right.size();
  }

  // In order traversal
  inorder(): T[] {
    if (this.root === null) {
      return [];
    }
    return [...this.root.left.inorder(), this.root.value, ...this.root.right.inorder()];
  }

  // String representation of the tree
  toString(): string {
    if (this.root === null) {
      return ""; 
    }
    const left = this.root.left instanceof Empty ? "" : this.root.left.toString();
    const right = this.root.right instanceof Empty ? "" : this.root.right.toString();
    return `(${left}${this.root.value}${right})`;
  }
}

export class Empty<T> extends BinarySearchTree<T> {
  constructor() {
    super(null);
  }

  inorder(): T[] {
    return [];
  }

  // Return "()" for an empty tree
  toString(): string {
    return "()";
  }
}
