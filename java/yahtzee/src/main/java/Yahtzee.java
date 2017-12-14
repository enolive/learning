import java.util.Arrays;
import java.util.Comparator;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Yahtzee {
    private final int[] dices;

    public Yahtzee(int... dices) {

        this.dices = dices;
    }

    public int ones() {
        return countEyesFor(1);
    }

    public int twos() {
        return countEyesFor(2);
    }

    public int threes() {
        return countEyesFor(3);
    }

    public int fours() {
        return countEyesFor(4);
    }

    public int fives() {
        return countEyesFor(5);
    }

    public int pair() {
        IntStream stream = Arrays.stream(dices);
        Map<Integer, Long> groupByEyes = stream
                .boxed()
                .collect(Collectors.groupingBy(
                        Function.identity(), Collectors.counting()));
        return groupByEyes.entrySet()
                  .stream()
                  .filter(e -> e.getValue() == 2)
                  .max(Comparator.comparing(Map.Entry::getKey))
                  .map(e -> e.getKey() * e.getValue())
                  .orElse(0L)
                  .intValue();
    }

    private int countEyesFor(int whichEye) {
        return (int) Arrays
                .stream(dices).filter(d -> d == whichEye)
                .count() * whichEye;
    }
}
