import java.util.Arrays;
import java.util.Map;
import java.util.Set;
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
        return (int) distinctEyes().filter(Yahtzee::isPair)
                                   .mapToLong(Yahtzee::eyesTimesOccurrence)
                                   .max()
                                   .orElse(0L);
    }

    public int twoPairs() {
        long[] pairValues = distinctEyes().filter(Yahtzee::isPair)
                                          .mapToLong(Yahtzee::eyesTimesOccurrence)
                                          .toArray();
        return pairValues.length != 2
                ? 0
                : (int) Arrays.stream(pairValues).sum();
    }

    public int threeOfAKind() {
        return (int) distinctEyes().filter(Yahtzee::areThreeOfAKind)
                                   .mapToLong(Yahtzee::eyesTimesOccurrence)
                                   .sum();
    }

    public int fourOfAKind() {
        return (int) distinctEyes().filter(Yahtzee::areFourOfAKind)
                                   .mapToLong(Yahtzee::eyesTimesOccurrence)
                                   .sum();
    }

    public int smallStraight() {
        return dicesContainAll(1, 2, 3, 4, 5)
                ? sumOfAllDices()
                : 0;
    }

    public int largeStraight() {
        return dicesContainAll(2, 3, 4, 5, 6)
                ? sumOfAllDices()
                : 0;
    }

    public int chance() {
        return sumOfAllDices();
    }

    public int fullHouse() {
        long[] pairAndTriple = distinctEyes()
                .filter(e -> isPair(e) || areThreeOfAKind(e))
                .mapToLong(Yahtzee::eyesTimesOccurrence)
                .toArray();
        return pairAndTriple.length == 2
                ? (int) Arrays.stream(pairAndTriple).sum()
                : 0;
    }

    private static boolean isPair(Map.Entry<Integer, Long> e) {
        return e.getValue() == 2;
    }

    private static Long eyesTimesOccurrence(Map.Entry<Integer, Long> e) {
        return e.getKey() * e.getValue();
    }

    private static boolean areThreeOfAKind(Map.Entry<Integer, Long> e) {
        return e.getValue() == 3;
    }

    private static boolean areFourOfAKind(Map.Entry<Integer, Long> e) {
        return e.getValue() == 4;
    }

    private int sumOfAllDices() {
        return Arrays.stream(dices).sum();
    }

    private boolean dicesContainAll(Integer... eyes) {
        Set<Integer> set = Arrays.stream(dices)
                                 .boxed()
                                 .collect(Collectors.toSet());
        return set.containsAll(Arrays.asList(eyes));
    }

    private Stream<Map.Entry<Integer, Long>> distinctEyes() {
        return Arrays
                .stream(dices)
                .boxed()
                .collect(Collectors.groupingBy(
                        Function.identity(), Collectors.counting()))
                .entrySet()
                .stream();
    }

    private int countEyesFor(int whichEye) {
        return (int) Arrays
                .stream(dices)
                .filter(d -> d == whichEye)
                .count() * whichEye;
    }
}
