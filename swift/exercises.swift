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
        let newPhrase = words.isEmpty ? nextWord : words + " " + nextWord
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
        let lines = contents.split(separator: "\n")

        let meaningfulLinesCount = lines.filter { line in
            let trimmedLine = line.trimmingCharacters(in: .whitespaces)
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

    static func + (leftSide: Quaternion, rightSide: Quaternion) -> Quaternion {
        return Quaternion(a: leftSide.a + rightSide.a, b: leftSide.b + rightSide.b, c: leftSide.c + rightSide.c, d: leftSide.d + rightSide.d)
    }

    static func * (leftSide: Quaternion, rightSide: Quaternion) -> Quaternion {
        return Quaternion(
            a: leftSide.a * rightSide.a - leftSide.b * rightSide.b - leftSide.c * rightSide.c - leftSide.d * rightSide.d,
            b: leftSide.a * rightSide.b + leftSide.b * rightSide.a + leftSide.c * rightSide.d - leftSide.d * rightSide.c,
            c: leftSide.a * rightSide.c - leftSide.b * rightSide.d + leftSide.c * rightSide.a + leftSide.d * rightSide.b,
            d: leftSide.a * rightSide.d + leftSide.b * rightSide.c - leftSide.c * rightSide.b + leftSide.d * rightSide.a
        )
    }

    var description: String{
        var result = ""
        if a != 0 {
            result += "\(a)"
        }
        if b != 0 {
            result += (result.isEmpty ? "" : (b > 0 ? "+" : "")) + "\(b)i"
        }
        if c != 0 {
            result += (result.isEmpty ? "" : (c > 0 ? "+" : "")) + "\(c)j"
        }
        if d != 0 {
            result += (result.isEmpty ? "" : (d > 0 ? "+" : "")) + "\(d)k"
        }
        return result.isEmpty ? "0" : result
    }

    // help from chat, passes two extra cases, investigating why
//     var description: String {
//     var result = ""

//     // Append the 'a' component if it is non-zero
//     if a != 0 {
//         result += "\(a)"
//     }

//     // Append the 'b' component with 'i' if it is non-zero
//     if b != 0 {
//         result += (result.isEmpty ? "" : (b > 0 ? "+" : "")) + "\(b)i"
//     }

//     // Append the 'c' component with 'j' if it is non-zero
//     if c != 0 {
//         result += (result.isEmpty ? "" : (c > 0 ? "+" : "")) + "\(c)j"
//     }

//     // Append the 'd' component with 'k' if it is non-zero
//     if d != 0 {
//         result += (result.isEmpty ? "" : (d > 0 ? "+" : "")) + "\(d)k"
//     }

//     // Handle cases where the quaternion is zero
//     if result.isEmpty {
//         return "0"
//     }

//     // Special formatting for the cases of 'i', 'j', and 'k'
//     if a == 0 && b == 1 && c == 0 && d == 0 {
//         return "i"
//     } else if a == 0 && b == 0 && c == 1 && d == 0 {
//         return "j"
//     } else if a == 0 && b == 0 && c == 0 && d == 1 {
//         return "k"
//     } else if a == 0 && b == 0 && c == -1 && d == 0 {
//         return "-j"
//     } else if a == 0 && b == 0 && c == 0 && d == -1 {
//         return "-k"
//     }

//     return result
// }
}

// Implement value-based equality
extension Quaternion: Equatable {
    static func == (lhs: Quaternion, rhs: Quaternion) -> Bool {
        return lhs.a == rhs.a && lhs.b == rhs.b && lhs.c == rhs.c && lhs.d == rhs.d
    }
}

// Conformance to CustomStringConvertible
extension Quaternion: CustomStringConvertible {}

// Write your Binary Search Tree enum here


