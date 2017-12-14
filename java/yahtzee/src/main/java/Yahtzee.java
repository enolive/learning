import java.util.Arrays;
import java.util.Comparator;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
        Map<Integer, Long> groupByEyes = Arrays
                .stream(dices)
                .boxed()
                .collect(Collectors.groupingBy(
                        Function.identity(), Collectors.counting()));
        Stream<Map.Entry<Integer, Long>> pairs = groupByEyes.entrySet()
                                                            .stream()
                                                            .filter(Yahtzee::isPair);
        return pairs
                .max(Comparator.comparing(Map.Entry::getKey))
                .map(Yahtzee::pairValue)
                .orElse(0L)
                .intValue();
    }

    public int twoPairs() {
        Map<Integer, Long> groupByEyes = Arrays
                .stream(dices)
                .boxed()
                .collect(Collectors.groupingBy(
                        Function.identity(), Collectors.counting()));
        long[] pairValues = groupByEyes.entrySet()
                                  .stream()
                                  .filter(Yahtzee::isPair)
                                  .mapToLong(Yahtzee::pairValue)
                                  .toArray();
        if (pairValues.length != 2) {
            return 0;
        }
        return (int) Arrays.stream(pairValues).sum();
    }

    private static boolean isPair(Map.Entry<Integer, Long> e) {
        return e.getValue() == 2;
    }

    private static Long pairValue(Map.Entry<Integer, Long> e) {
        return e.getKey() * e.getValue();
    }

    private int countEyesFor(int whichEye) {
        return (int) Arrays
                .stream(dices).filter(d -> d == whichEye)
                .count() * whichEye;
    }
}
