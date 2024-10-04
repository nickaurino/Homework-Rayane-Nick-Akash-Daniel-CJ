import Foundation

struct NegativeAmountError: Error {}
struct NoSuchFileError: Error {}

func change(_ amount: Int) -> Result<[Int:Int], NegativeAmountError> {
    if amount < 0 {
        return .failure(NegativeAmountError())
    }
    var (counts, remaining) = ([Int:Int](), amount)
    for denomination in [25, 10, 5, 1] {
        (counts[denomination], remaining) = 
            remaining.quotientAndRemainder(dividingBy: denomination)
    }
    return .success(counts)
}

// Write your first then lower case function here
func firstThenLowerCase(of a: [String], satisfying p: (String) -> Bool) -> String? {
    return a.first(where: p)?.lowercased()
}

// Write your say function here
class Say {
    private let words: String

    init(_ initialWord: String = "") {
        self.words = initialWord
    }

    func and(_ nextWord: String) -> Say {
        let newPhrase: String
        if words.isEmpty && nextWord.isEmpty {
            newPhrase = " "
        } else if words.isEmpty {
            newPhrase = nextWord
        } else if nextWord.isEmpty {
            newPhrase = words + " "
        } else {
            newPhrase = words + " " + nextWord
        }
        return Say(newPhrase)
    }

    var phrase: String {
        return words
    }
}

func say(_ initialWord: String = "") -> Say {
    return Say(initialWord)
}

// Write your meaningfulLineCount function here
func meaningfulLineCount(_ filename: String) async -> Result<Int, Error> {
    do {
        let fileURL = URL(fileURLWithPath: filename)
        let fileHandle = try FileHandle(forReadingFrom: fileURL)
        defer {
            //close
            try? fileHandle.close()
        }

        let contents = String(data: fileHandle.readDataToEndOfFile(), encoding: .utf8) ?? ""
        let lines = contents.split(whereSeparator: \.isNewline)

        let meaningfulLinesCount = lines.filter { line in
            let trimmedLine = line.trimmingCharacters(in: .whitespacesAndNewlines)
            return !trimmedLine.isEmpty && !trimmedLine.hasPrefix("#")
        }.count

        return .success(meaningfulLinesCount)
    } catch { 
        return .failure(error)
    }
}

// Write your Quaternion struct here
struct Quaternion {
    let a: Double
    let b: Double
    let c: Double
    let d: Double

    static let ZERO = Quaternion(a: 0, b: 0, c: 0, d: 0)
    static let I = Quaternion(a: 0, b: 1, c: 0, d: 0)
    static let J = Quaternion(a: 0, b: 0, c: 1, d: 0)
    static let K = Quaternion(a: 0, b: 0, c: 0, d: 1)

    // Quat constructor
    init(a: Double = 0, b: Double = 0, c: Double = 0, d: Double = 0) {
        self.a = a
        self.b = b
        self.c = c
        self.d = d
    }

    var coefficients: [Double] {
        return [a, b, c, d]
    }

    var conjugate: Quaternion {
        return Quaternion(a: a, b: -b, c: -c, d: -d)
    }

    // addition functionality
    static func + (leftSide: Quaternion, rightSide: Quaternion) -> Quaternion {
        return Quaternion(a: leftSide.a + rightSide.a, b: leftSide.b + rightSide.b, c: leftSide.c + rightSide.c, d: leftSide.d + rightSide.d)
    }

    // multipilication functionality
    static func * (leftSide: Quaternion, rightSide: Quaternion) -> Quaternion {
        return Quaternion(
            a: leftSide.a * rightSide.a - leftSide.b * rightSide.b - leftSide.c * rightSide.c - leftSide.d * rightSide.d,
            b: leftSide.a * rightSide.b + leftSide.b * rightSide.a + leftSide.c * rightSide.d - leftSide.d * rightSide.c,
            c: leftSide.a * rightSide.c - leftSide.b * rightSide.d + leftSide.c * rightSide.a + leftSide.d * rightSide.b,
            d: leftSide.a * rightSide.d + leftSide.b * rightSide.c - leftSide.c * rightSide.b + leftSide.d * rightSide.a
        )
    }
    
    // String representation of Quat
    var description: String{
        var result = ""
        if a != 0 {
            result += "\(a)"
        }

        if b != 0 {
            if !result.isEmpty && b > 0 {
                result += "+"
            }
            // directly return "i" and not 1i, handles conjugate
            result += b == 1 ? "i" : (b == -1 ? "-i" : "\(b)i")
        }

        if c != 0 {
            if !result.isEmpty && c > 0 {
                result += "+"
            }
            result += c == 1 ? "j" : (c == -1 ? "-j" : "\(c)j")
        }

        if d != 0 {
            if !result.isEmpty && d > 0 {
                result += "+"
            }
            result += d == 1 ? "k" : (d == -1 ? "-k" : "\(d)k")
        }
        return result.isEmpty ? "0" : result
    }
}

// override equality functionalty for our struct
extension Quaternion: Equatable {
    static func == (lhs: Quaternion, rhs: Quaternion) -> Bool {
        return lhs.a == rhs.a && lhs.b == rhs.b && lhs.c == rhs.c && lhs.d == rhs.d
    }
}

// CustomStringConvertible to convert string to correct format
extension Quaternion: CustomStringConvertible {}

// Write your Binary Search Tree enum here
indirect enum BinarySearchTree {
    case empty
    case node(left: BinarySearchTree, value: String, right: BinarySearchTree)

    // BST computed size 
    var size: Int {
        switch self {
        case .empty:
            return 0
        case let .node(left, _, right):
            return 1 + left.size + right.size
        }
    }

    // Insert a value
    func insert(_ newValue: String) -> BinarySearchTree {
        switch self {
        case .empty:
            return .node(left: .empty, value: newValue, right: .empty)
        case let .node(left, value, right):
            if newValue < value {
                return .node(left: left.insert(newValue), value: value, right: right)
            } else if newValue > value {
                return .node(left: left, value: value, right: right.insert(newValue))
            } else {
                return self // Value already exists, return unchanged tree
            }
        }
    }

    // Check if a value exists
    func contains(_ searchValue: String) -> Bool {
        switch self {
        case .empty:
            return false
        case let .node(left, value, right):
            if searchValue < value {
                return left.contains(searchValue)
            } else if searchValue > value {
                return right.contains(searchValue)
            } else {
                return true // Found the value
            }
        }
    }

    // String representation of the BST
    var description: String {
        switch self {
        case .empty:
            return "()"
        case let .node(left, value, right):
            let leftDesc = left.isEmpty ? "" : left.description
            let rightDesc = right.isEmpty ? "" : right.description
        
            if leftDesc.isEmpty && rightDesc.isEmpty {
            return "(\(value))"
            } else {
                return "(\(leftDesc)\(value)\(rightDesc))"
            }
        }
    }

    // Helper to check tree if empty
    var isEmpty: Bool {
        if case .empty = self {
            return true
        }
        return false
    }
    
    // override to implement equals
    static func == (lhs: BinarySearchTree, rhs: BinarySearchTree) -> Bool {
        switch (lhs, rhs) {
        case (.empty, .empty):
            return true
        case let (.node(left1, value1, right1), .node(left2, value2, right2)):
            return value1 == value2 && left1 == left2 && right1 == right2
        default:
            return false
        }
    }
}

// CustomStringConvertible to convert string to correct format
extension BinarySearchTree: CustomStringConvertible {}

