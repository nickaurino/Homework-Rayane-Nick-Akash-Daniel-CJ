import java.io.BufferedReader
import java.io.FileReader
import java.io.IOException

fun change(amount: Long): Map<Int, Long> {
    require(amount >= 0) { "Amount cannot be negative" }
    
    val counts = mutableMapOf<Int, Long>()
    var remaining = amount
    for (denomination in listOf(25, 10, 5, 1)) {
        counts[denomination] = remaining / denomination
        remaining %= denomination
    }
    return counts
}

// Write your first then lower case function here
fun firstThenLowerCase(a: List<String>, p: (String) -> Boolean): String? {
    return a.firstOrNull(p)?.lowercase()
}

// Write your say function here
class Say(private val words: String = "") {

    // method to add strings
    fun and(nextWord: String): Say {
        val newPhrase = if (words.isEmpty()) nextWord else "$words $nextWord"
        if (nextWord.isEmpty()) return Say(words + " ")
        return Say(newPhrase)
    }

    val phrase: String
        get() = words
}

fun say(initialWord: String = ""): Say {
    return Say(initialWord)
}

// Write your meaningfulLineCount function here
@Throws(IOException::class)
fun meaningfulLineCount(filename: String): Long {
    BufferedReader(FileReader(filename)).use { reader -> 
        return reader.lineSequence()
            .filter { it.isNotBlank() && !it.trimStart().startsWith("#") }
            .count()
            .toLong()
    }
}


// Write your Quaternion data class here
data class Quaternion private constructor(
    val a: Double,
    val b: Double,
    val c: Double,
    val d: Double
) {

    companion object {
        val ZERO = Quaternion(0.0, 0.0, 0.0, 0.0)
        val I = Quaternion(0.0, 1.0, 0.0, 0.0)
        val J = Quaternion(0.0, 0.0, 1.0, 0.0)
        val K = Quaternion(0.0, 0.0, 0.0, 1.0)

        operator fun invoke(a: Double, b: Double, c: Double, d: Double): Quaternion {
            return Quaternion(a, b, c, d)
        }
    }

    // quat addition
    operator fun plus(other: Quaternion): Quaternion {
        return Quaternion(
            this.a + other.a,
            this.b + other.b,
            this.c + other.c,
            this.d + other.d
        )
    }

    // quat multiplication
    operator fun times(other: Quaternion): Quaternion {
        val newA = this.a * other.a - this.b * other.b - this.c * other.c - this.d * other.d
        val newB = this.a * other.b + this.b * other.a + this.c * other.d - this.d * other.c
        val newC = this.a * other.c - this.b * other.d + this.c * other.a + this.d * other.b
        val newD = this.a * other.d + this.b * other.c - this.c * other.b + this.d * other.a
        return Quaternion(newA, newB, newC, newD)
    }

    fun coefficients(): List<Double> {
        return listOf(a, b, c, d)
    }

    // Conjugate
    fun conjugate(): Quaternion {
        return Quaternion(a, -b, -c, -d)
    } 

    override fun toString(): String {
        val sb = StringBuilder()

        if (a != 0.0) sb.append(a)
        
        if (b == 1.0) sb.append(if (sb.isNotEmpty()) "+" else "").append("i")
        else if (b == -1.0) sb.append("-i")
        else if (b != 0.0) sb.append(if (b > 0 && sb.isNotEmpty()) "+" else "").append("${b}i")

        if (c == 1.0) sb.append(if (sb.isNotEmpty()) "+" else "").append("j")
        else if (c == -1.0) sb.append("-j")
        else if (c != 0.0) sb.append(if (c > 0 && sb.isNotEmpty()) "+" else "").append("${c}j")

        if (d == 1.0) sb.append(if (sb.isNotEmpty()) "+" else "").append("k")
        else if (d == -1.0) sb.append("-k")
        else if (d != 0.0) sb.append(if (d > 0 && sb.isNotEmpty()) "+" else "").append("${d}k")
        
        return if (sb.isEmpty()) "0" else sb.toString()
    }
    
}

// Write your Binary Search Tree interface and implementing classes here
sealed interface BinarySearchTree {
    fun size(): Int
    fun contains(value: String): Boolean
    fun insert(value: String): BinarySearchTree

    // To convert tree into String
    override fun toString(): String

    object Empty : BinarySearchTree {
        override fun size(): Int = 0
        override fun contains(value: String): Boolean = false
        override fun insert(value: String): BinarySearchTree = Node(value, Empty, Empty)
        override fun toString(): String = "()"
    }

    data class Node(
        val value: String,
        val left: BinarySearchTree,
        val right: BinarySearchTree
    ) : BinarySearchTree {
        override fun size(): Int = 1 + left.size() + right.size()
        
        // check if tree contains string
        override fun contains(value: String): Boolean {
            return when {
                this.value == value -> true
                value < this.value -> left.contains(value)
                else -> right.contains(value)
            }
        }

        // insert String into tree
        override fun insert(value: String): BinarySearchTree {
            return when {
                value < this.value -> copy(left = left.insert(value))
                value > this.value -> copy(right = right.insert(value))
                else -> this // val exists leave as is
            }
        }
        
        override fun toString(): String {
            val leftStr = if (left == Empty) "" else left.toString()
            val rightStr = if (right == Empty) "" else right.toString()
            return "($leftStr$value$rightStr)"
        }
    }
}
