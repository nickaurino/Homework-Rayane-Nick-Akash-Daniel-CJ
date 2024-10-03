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
