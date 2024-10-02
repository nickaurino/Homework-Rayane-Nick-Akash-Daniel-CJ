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
    public static Optional<String> findFirstLowercase(List<String> list, Predicate<String> predicate) {
        return list.stream()
                .filter(predicate)
                .findFirst()
                .map(String::toLowerCase);
    }

    // Write your say function here
    public class Say {
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

        public String getPhrase() {
            return phrase.toString();
        }

        public static Say say(String word) {
            return new Say(word);
        }
    }

    // Write your line count function here
    public static long countValidLines(String filename) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            return reader.lines()
                    .filter(line -> !line.trim().isEmpty()) // Not empty or whitespace
                    .filter(line -> !line.trim().startsWith("#")) // Does not start with #
                    .count(); // Count valid lines
        }
    }
}

// Write your Quaternion record class here
public record Quaternion(double w, double x, double y, double z) {

    public static final Quaternion ZERO = new Quaternion(0, 0, 0, 0);
    public static final Quaternion I = new Quaternion(0, 1, 0, 0);
    public static final Quaternion J = new Quaternion(0, 0, 1, 0);
    public static final Quaternion K = new Quaternion(0, 0, 0, 1);

    public Quaternion add(Quaternion other) {
        return new Quaternion(this.w + other.w, this.x + other.x, this.y + other.y, this.z + other.z);
    }

    public Quaternion multiply(Quaternion other) {
        double newW = this.w * other.w - this.x * other.x - this.y * other.y - this.z * other.z;
        double newX = this.w * other.x + this.x * other.w + this.y * other.z - this.z * other.y;
        double newY = this.w * other.y - this.x * other.z + this.y * other.w + this.z * other.x;
        double newZ = this.w * other.z + this.x * other.y - this.y * other.x + this.z * other.w;
        return new Quaternion(newW, newX, newY, newZ);
    }

    public Quaternion conjugate() {
        return new Quaternion(w, -x, -y, -z);
    }

    public double[] getCoefficients() {
        return new double[]{w, x, y, z};
    }

    @Override
    public String toString() {
        return String.format("(%f, %f, %f, %f)", w, x, y, z);
    }
}

// Write your BinarySearchTree sealed interface and its implementations here
public sealed interface BinarySearchTree permits Empty, Node {
    BinarySearchTree insert(String value);
    boolean contains(String value);
    int size();
    String toString();
}

public final class Empty implements BinarySearchTree {
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

public final class Node implements BinarySearchTree {
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
            return new Node(this.value, left.insert(value), right);
        } else if (value.compareTo(this.value) > 0) {
            return new Node(this.value, left, right.insert(value));
        } else {
            return this;
        }
    }

    @Override
    public boolean contains(String value) {
        if (value.equals(this.value)) {
            return true;
        } else if (value.compareTo(this.value) < 0) {
            return left.contains(value);
        } else {
            return right.contains(value);
        }
    }

    @Override
    public int size() {
        return 1 + left.size() + right.size();
    }

    @Override
    public String toString() {
        return String.format("(%s)%s(%s)", left, value, right);
    }
}
