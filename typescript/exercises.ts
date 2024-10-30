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
export function firstThenApply<T, U>(arr: T[], predicate: (element: T) => boolean, fn: (element: T) => U): U | undefined {
  const found = arr.find(predicate);
  return found !== undefined ? fn(found) : undefined;
}
// Write your powers generator here
export function* powersGenerator(base: bigint): Generator<bigint, void, undefined> {
  let current = 1n;
  while (true) {
    yield current;
    current *= base;
  }
}
// Write your line count function here
export async function meaningfulLineCount(filePath: string): Promise<number> {
  const file = await open(filePath, "r");
  let count = 0;

  for await (const line of file.readLines()) {
    if (line.trim() && !line.trim().startsWith("#")) {
      count++;
    }
  }

  await file.close();
  return count;
}
// Write your shape type and associated functions here
type Shape =
  | { kind: "Box"; width: number; length: number; depth: number }
  | { kind: "Sphere"; radius: number };

export function volume(shape: Shape): number {
  if (shape.kind === "Box") {
    return shape.width * shape.length * shape.depth;
  } else {
    return (4 / 3) * Math.PI * Math.pow(shape.radius, 3);
  }
}

export function surfaceArea(shape: Shape): number {
  if (shape.kind === "Box") {
    return 2 * (shape.width * shape.length + shape.length * shape.depth + shape.width * shape.depth);
  } else {
    return 4 * Math.PI * Math.pow(shape.radius, 2);
  }
}
// Write your binary search tree implementation here
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
      return this; // Value already exists, no change
    }
  }

  contains(value: T): boolean {
    if (this.root === null) {
      return false;
    }
    if (value === this.root.value) {
      return true;
    } else if (value < this.root.value) {
      return this.root.left.contains(value);
    } else {
      return this.root.right.contains(value);
    }
  }

  size(): number {
    if (this.root === null) {
      return 0;
    }
    return 1 + this.root.left.size() + this.root.right.size();
  }

  inorder(): T[] {
    if (this.root === null) {
      return [];
    }
    return [...this.root.left.inorder(), this.root.value, ...this.root.right.inorder()];
  }

  toString(): string {
    if (this.root === null) {
      return "";
    }
    const left = this.root.left.toString();
    const right = this.root.right.toString();
    return `(${left ? left : ""}${this.root.value}${right ? right : ""})`;
  }
}

export class Empty<T> extends BinarySearchTree<T> {
  constructor() {
    super(null);
  }

  inorder(): T[] {
    return [];
  }
}
