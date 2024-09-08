from dataclasses import dataclass
from collections.abc import Callable
import os

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
def check_file_path(file_path: str):
    try:
        # Get the absolute path from the relative path
        absolute_path = os.path.abspath(file_path)

        # Print the working directory and the absolute path
        print(f"Current Working Directory: {os.getcwd()}")
        print(f"Absolute path of the file: {absolute_path}")

        # Check if the file exists
        if not os.path.exists(absolute_path):
            print(f"File does not exist at: {absolute_path}")
        else:
            print(f"File found at: {absolute_path}")
            # Try to print the content of the file
            with open(absolute_path, 'r') as file:
                content = file.read()
                print(content)
    except FileNotFoundError as e:
        print(f"FileNotFoundError: {e}")

# Call the function with the relative path for testing.
check_file_path("../test-for-line-count.txt")

# def meaningful_line_count(file_path: str) -> int:
#     count = 0
#     try:
#         # Print the current working directory for debugging
#         print(f"Current Working Directory: {os.getcwd()}")
        
#         # Check if the file exists at the given path
#         if not os.path.exists(file_path):
#             raise FileNotFoundError(f"No such file: '{file_path}'")
        
#         with open(file_path, 'r') as file:
#             for line in file:
#                 stripped_line = line.strip()
#                 if stripped_line and not stripped_line.startswith('#'):
#                     count += 1
#     except FileNotFoundError as e:
#         # Propagate the error with a custom message if necessary
#         raise FileNotFoundError(f"No such file: '{file_path}'") from e
#     return count
# print(meaningful_line_count("../test-for-line-count.txt"))
# print(meaningful_line_count("no-such-file.txt"))
# Write your Quaternion class here
