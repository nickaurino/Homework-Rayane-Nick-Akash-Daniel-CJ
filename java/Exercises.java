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
                .findFirst()
                .map(String::toLowerCase);
    }

    // Write your say function here
    static class Say {
        private final StringBuilder phrase;

        public Say(String word) {
            this.phrase = new StringBuilder(word);
        }

        public Say and(String word) {
            if (phrase.length() > 0) {
                phrase.append(" ");
            }
            phrase.append(word);
            return this;
        }

        public String phrase() {
            return phrase.toString();
        }


    }

    static Say say(String word) {
        return new Say(word);
    }

    static Say say() {
        return new Say("");
    }

    // Write your line count function here
    public static long meaningfulLineCount(String filename) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            return reader.lines()
                    .filter(line -> !line.trim().isEmpty()) // Not empty or whitespace
                    .filter(line -> !line.trim().startsWith("#")) // Does not start with #
                    .count(); // Count valid lines
        }
    }
}

// Write your Quaternion record class here
record Quaternion(double a, double b, double c, double d) {



    public static final Quaternion ZERO = new Quaternion(0, 0, 0, 0);
    public static final Quaternion I = new Quaternion(0, 1, 0, 0);
    public static final Quaternion J = new Quaternion(0, 0, 1, 0);
    public static final Quaternion K = new Quaternion(0, 0, 0, 1);

    public Quaternion {
        if (Double.isNaN(a) || Double.isNaN(b) || Double.isNaN(c) || Double.isNaN(d)){
            throw new IllegalArgumentException("Coefficients cannot be NaN");
        }
    }

    public Quaternion plus(Quaternion other) {
        return new Quaternion(this.a + other.a, this.b + other.b, this.c + other.c, this.d + other.d);
    }

    public Quaternion times(Quaternion other) {
        double newW = this.a * other.a - this.b * other.b - this.c * other.c - this.d * other.d;
        double newX = this.a* other.b + this.b * other.a + this.c * other.d - this.d * other.c;
        double newY = this.a * other.c - this.b * other.d + this.c * other.a + this.d * other.b;
        double newZ = this.a * other.d + this.b * other.c - this.c * other.b + this.d * other.a;
        return new Quaternion(newW, newX, newY, newZ);
    }

    public Quaternion conjugate() {
        return new Quaternion(a, -b, -c, -d);
    }

    public List<Double> coefficients() {
        return List.copyOf(List.of(a,b,c,d));
    }

    @Override
    public String toString() {
        StringBuilder parts = new StringBuilder();

        if (this.a != 0) parts.append(String.format("%.1f", this.a));
        if (this.b != 0) {
            if (parts.length() > 0) parts.append(this.b > 0 ? "+" : "");
            parts.append(this.b == 1 ? "i" : (this.b == -1 ? "-i" : String.format("%.1f", this.b) + "i"));
        }
        if (this.c != 0) {
            if (parts.length() > 0) parts.append(this.c > 0 ? "+" : "");
            parts.append(this.c == 1 ? "j" : (this.c == -1 ? "-j" : String.format("%.1f", this.c) + "j"));
        }
        if (this.d != 0) {
            if (parts.length() > 0) parts.append(this.d > 0 ? "+" : "");
            parts.append(this.d == 1 ? "k" : (this.d == -1 ? "-k" : String.format("%.1f", this.d) + "k"));
        }

        String result = parts.toString();
        result = result.replace("+-", "-");

        return result.isEmpty() ? "0" : result;
    }
    
}

// Write your BinarySearchTree sealed interface and its implementations here
sealed interface BinarySearchTree permits Empty, Node {
    BinarySearchTree insert(String value);
    boolean contains(String value);
    int size();
    String toString();
}

final class Empty implements BinarySearchTree {
    public static final Empty INSTANCE = new Empty();

    private Empty() {}

    @Override
    public BinarySearchTree insert(String value) {
        return new Node(value, this, this);
    }

    @Override
    public boolean contains(String value) {
        return false;
    }

    @Override
    public int size() {
        return 0;
    }

    @Override
    public String toString() {
        return "";
    }
}


final class Node implements BinarySearchTree {
    private final String value;
    private final BinarySearchTree left;
    private final BinarySearchTree right;

    public Node(String value, BinarySearchTree left, BinarySearchTree right) {
        this.value = value;
        this.left = left;
        this.right = right;
    }


    @Override
    public BinarySearchTree insert(String value) {
        if (value.compareTo(this.value) < 0) {
            // Insert into the left subtree
            return new Node(this.value, left.insert(value), right);
        } else if (value.compareTo(this.value) > 0) {
            // Insert into the right subtree
            return new Node(this.value, left, right.insert(value));
        } else {
            // If the value already exists, return the current node (no duplicates)
            return this;
        }
    }

    @Override
    public boolean contains(String value) {
        if (value.equals(this.value)) {
            return true;
        } else if (value.compareTo(this.value) < 0) {
            // Search in the left subtree
            return left.contains(value);
        } else {
            // Search in the right subtree
            return right.contains(value);
        }
    }

    @Override
    public int size() {
        // Size is the current node (1) + size of left + size of right
        return 1 + left.size() + right.size();
    }

    @Override
    public String toString() {
        // Represent the tree structure, using in-order traversal
        return String.format("(%s)%s(%s)", left, value, right);
    }
}

