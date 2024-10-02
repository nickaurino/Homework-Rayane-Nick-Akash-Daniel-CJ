import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Optional;
import java.util.function.Predicate;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Exercises {
    static Map<Integer, Long> change(long amount) {
        if (amount < 0) {
            throw new IllegalArgumentException("Amount cannot be negative");
        }
        var counts = new HashMap<Integer, Long>();
        for (var denomination : List.of(25, 10, 5, 1)) {
            counts.put(denomination, amount / denomination);
            amount %= denomination;
        }
        return counts;
    }

    // Write your first then lower case function here
    public static Optional<String> firstThenLowerCase(List<String> list, Predicate<String> predicate) {
        return list.stream()
                   .filter(predicate)
                   .map(String::toLowerCase)
                   .findFirst();
    }

    // say function implementation (modified to handle no argument case)
    private StringBuilder phraseBuilder;

    private Exercises(String initialWord) {
        this.phraseBuilder = new StringBuilder(initialWord);
    }

    public static Exercises say() {
        return new Exercises("");  // For cases with no arguments
    }

    public static Exercises say(String word) {
        return new Exercises(word.trim());  // Initialize phrase with the first word (trimmed)
    }

    public Exercises and(String word) {
        if (!phraseBuilder.isEmpty()) {
            phraseBuilder.append(" ");
        }
        phraseBuilder.append(word.trim());  // Append next word with space
        return this;
    }

    public String phrase() {
        return phraseBuilder.toString();  // Return accumulated phrase
    }

    // Write your line count function here
    public static long meaningfulLineCount(String filename) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            return reader.lines()
                    .filter(line -> !line.trim().isEmpty())  // Filter out empty lines or whitespace-only lines
                    .filter(line -> !line.trim().startsWith("#"))  // Filter out lines that start with #
                    .count();  // Count the remaining valid lines
        }
    }
}

// Write your Quaternion record class here

public record Quaternion(double a, double b, double c, double d) {

    public static final Quaternion ZERO = new Quaternion(0, 0, 0, 0);
    public static final Quaternion I = new Quaternion(0, 1, 0, 0);
    public static final Quaternion J = new Quaternion(0, 0, 1, 0);
    public static final Quaternion K = new Quaternion(0, 0, 0, 1);

    // Constructor with NaN validation
    public Quaternion {
        if (Double.isNaN(a) || Double.isNaN(b) || Double.isNaN(c) || Double.isNaN(d)) {
            throw new IllegalArgumentException("Coefficients cannot be NaN");
        }
    }

    // Addition (Quaternion + Quaternion)
    public Quaternion plus(Quaternion other) {
        return new Quaternion(
                this.a + other.a,
                this.b + other.b,
                this.c + other.c,
                this.d + other.d
        );
    }

    // Multiplication (Quaternion * Quaternion)
    public Quaternion times(Quaternion other) {
        double na = this.a * other.a - this.b * other.b - this.c * other.c - this.d * other.d;
        double nb = this.a * other.b + this.b * other.a + this.c * other.d - this.d * other.c;
        double nc = this.a * other.c - this.b * other.d + this.c * other.a + this.d * other.b;
        double nd = this.a * other.d + this.b * other.c - this.c * other.b + this.d * other.a;
        return new Quaternion(na, nb, nc, nd);
    }

    // Coefficients as a list
    public List<Double> coefficients() {
        return List.of(a, b, c, d);
    }

    // Conjugate (change signs of imaginary parts)
    public Quaternion conjugate() {
        return new Quaternion(a, -b, -c, -d);
    }

    // Custom string representation according to the tests
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();

        if (a != 0) {
            sb.append(a);
        }

        if (b > 0) {
            sb.append("+").append(b).append("i");
        } else if (b < 0) {
            sb.append(b).append("i");
        }

        if (c > 0) {
            sb.append("+").append(c).append("j");
        } else if (c < 0) {
            sb.append(c).append("j");
        }

        if (d > 0) {
            sb.append("+").append(d).append("k");
        } else if (d < 0) {
            sb.append(d).append("k");
        }

        // Special case for 0
        if (sb.length() == 0) {
            return "0";
        }

        return sb.toString();
    }

    // Standard value-based equality and hashCode come with record.
}

// Write your BinarySearchTree sealed interface and its implementations here
