import java.util.Arrays;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Yahtzee {
    private final int[] dices;

    Yahtzee(int... dices) {
        throwOnInvalid(dices);
        this.dices = dices;
    }

    private void throwOnInvalid(int[] dices) {
        if (!hasValidLength(dices)) {
            throw new IllegalArgumentException("The number of dices must be 5.");
        }
        if (Arrays.stream(dices).anyMatch(dice -> !hasValidEyeCount(dice))) {
            throw new IllegalArgumentException("The eye count for each dice must be between 1 and 6.");
        }
    }

    private boolean hasValidLength(int[] dices) {
        return dices.length == 5;
    }

    private boolean hasValidEyeCount(int dice) {
        return dice >= 1 && dice <= 6;
    }

    private static boolean isPair(Map.Entry<Integer, Long> e) {
        return e.getValue() == 2;
    }

    private static boolean areThreeOfAKind(Map.Entry<Integer, Long> e) {
        return e.getValue() == 3;
    }

    private static Long eyesTimesOccurrence(Map.Entry<Integer, Long> e) {
        return e.getKey() * e.getValue();
    }

    private static boolean areFourOfAKind(Map.Entry<Integer, Long> e) {
        return e.getValue() == 4;
    }

    private static boolean isYahtzee(Map.Entry<Integer, Long> e) {
        return e.getValue() == 5;
    }

    int ones() {
        return countEyesFor(1);
    }

    private int countEyesFor(int whichEye) {
        return (int) Arrays.stream(dices)
                           .filter(d -> d == whichEye)
                           .count() * whichEye;
    }

    int twos() {
        return countEyesFor(2);
    }

    int threes() {
        return countEyesFor(3);
    }

    int fours() {
        return countEyesFor(4);
    }

    int fives() {
        return countEyesFor(5);
    }

    int twoPairs() {
        long[] pairValues = distinctEyes().filter(Yahtzee::isPair)
                                          .mapToLong(Yahtzee::eyesTimesOccurrence)
                                          .toArray();
        return pairValues.length != 2
                ? 0
                : (int) Arrays.stream(pairValues).sum();
    }

    private Stream<Map.Entry<Integer, Long>> distinctEyes() {
        return Arrays.stream(dices)
                     .boxed()
                     .collect(Collectors.groupingBy(
                             Function.identity(), Collectors.counting()))
                     .entrySet()
                     .stream();
    }

    int fourOfAKind() {
        return (int) distinctEyes().filter(Yahtzee::areFourOfAKind)
                                   .mapToLong(Yahtzee::eyesTimesOccurrence)
                                   .findFirst()
                                   .orElse(0L);
    }

    int smallStraight() {
        return dicesContainAll(1, 2, 3, 4, 5)
                ? sumOfAllDices()
                : 0;
    }

    private boolean dicesContainAll(Integer... eyes) {
        Set<Integer> set = Arrays.stream(dices)
                                 .boxed()
                                 .collect(Collectors.toSet());
        return set.containsAll(Arrays.asList(eyes));
    }

    private int sumOfAllDices() {
        return Arrays.stream(dices).sum();
    }

    int largeStraight() {
        return dicesContainAll(2, 3, 4, 5, 6)
                ? sumOfAllDices()
                : 0;
    }

    int chance() {
        return sumOfAllDices();
    }

    int fullHouse() {
        int pair = pair();
        int triple = threeOfAKind();
        return triple > 0 && pair > 0
                ? pair + triple
                : 0;
    }

    int pair() {
        return (int) distinctEyes().filter(Yahtzee::isPair)
                                   .mapToLong(Yahtzee::eyesTimesOccurrence)
                                   .max()
                                   .orElse(0L);
    }

    int threeOfAKind() {
        return (int) distinctEyes().filter(Yahtzee::areThreeOfAKind)
                                   .mapToLong(Yahtzee::eyesTimesOccurrence)
                                   .findFirst()
                                   .orElse(0L);
    }

    int yahtzee() {
        return (int) distinctEyes().filter(Yahtzee::isYahtzee)
                                   .mapToLong(Yahtzee::eyesTimesOccurrence)
                                   .findFirst()
                                   .orElse(0L);
    }
}
