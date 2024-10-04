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

    fun and(nextWord: String): Say {
        val newPhrase = if (words.isEmpty()) nextWord else "$words $nextWord"
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
        
        override fun contains(value: String): Boolean {
            return when {
                this.value == value -> true
                value < this.value -> left.contains(value)
                else -> right.contains(value)
            }
        }

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
