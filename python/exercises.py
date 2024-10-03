from dataclasses import dataclass
from collections.abc import Callable


def change(amount: int) -> dict[int, int]:
    if not isinstance(amount, int):
        raise TypeError('Amount must be an integer')
    if amount < 0:
        raise ValueError('Amount cannot be negative')
    counts, remaining = {}, amount
    for denomination in (25, 10, 5, 1):
        counts[denomination], remaining = divmod(remaining, denomination)
    return counts


# Write your first then lower case function here
def first_then_lower_case(a: list[str], p: Callable[[str], bool]) -> str:
    for s in a:
        if p(s):
            return s.lower()
    return None


# Write your powers generator here

def powers_generator(*, base, limit):
    number = 1
    while number <= limit:
        yield number
        number *= base
        
# Write your say function here
def say(word=None):
    # Base case: if called without an argument, return an empty string
    if word is None:
        return ""
    
    # Recursive function to accumulate words
    def inner_say(next_word=None, words=[word]):
        if next_word is None:
            return ' '.join(words)
        else:
            words.append(next_word)
            return lambda w=None: inner_say(w, words)
    
    return inner_say

# Write your line count function here
def meaningful_line_count(file_path: str):
        count = 0
        try:
            with open(file_path, "r") as file:
                for line in file:
                    stripped_line = line.strip()
                    # Check if the line is not empty and does not start with '#'
                    if stripped_line and not stripped_line.startswith('#'):
                        count += 1
        except FileNotFoundError:
            raise FileNotFoundError("No such file")
        
        return count
# Write your Quaternion class here
class Quaternion:
    def __init__(self, a: float, b: float, c: float, d: float) -> None:
        self.a = a
        self.b = b
        self.c = c
        self.d = d
    
    def __add__(self, other):
        return Quaternion(self.a + other.a, self.b + other.b, self.c + other.c, self.d + other.d)
    
    def __mul__(self, other):
        if isinstance(other, Quaternion):
            # Quaternion multiplication formula
            a = self.a * other.a - self.b * other.b - self.c * other.c - self.d * other.d
            b = self.a * other.b + self.b * other.a + self.c * other.d - self.d * other.c
            c = self.a * other.c - self.b * other.d + self.c * other.a + self.d * other.b
            d = self.a * other.d + self.b * other.c - self.c * other.b + self.d * other.a
            return Quaternion(a, b, c, d)
        elif isinstance(other, (int, float)):
            # Scalar multiplication
            return Quaternion(self.a * other, self.b * other, self.c * other, self.d * other)
        else:
            raise TypeError("Multiplication only supports Quaternion or scalar types.")
    
    @property
    def conjugate(self):
        """Return the conjugate of the quaternion."""
        return Quaternion(self.a, -self.b, -self.c, -self.d)
    
    def __eq__(self, other):
        """Equality check for quaternions."""
        return (self.a == other.a and self.b == other.b and self.c == other.c and self.d == other.d)
    
    @property
    def coefficients(self):
        """Return the coefficients (a, b, c, d) as a tuple."""
        return (self.a, self.b, self.c, self.d)
    
    def __repr__(self):
        parts = []
        
        # Scalar part (a)
        if self.a != 0:
            parts.append(f"{self.a}")

        # Imaginary parts (b, c, d)
        if self.b != 0:
            if self.b == 1:
                parts.append("+i")
            elif self.b == -1:
                parts.append("-i")
            else:
                parts.append(f"{self.b:+}i")
        
        if self.c != 0:
            if self.c == 1:
                parts.append("+j")
            elif self.c == -1:
                parts.append("-j")
            else:
                parts.append(f"{self.c:+}j")

        if self.d != 0:
            if self.d == 1:
                parts.append("+k")
            elif self.d == -1:
                parts.append("-k")
            else:
                parts.append(f"{self.d:+}k")
        
        # Join the parts together and remove any leading "+" signs
        result = "".join(parts)
        return result.lstrip('+') if result else '0'
