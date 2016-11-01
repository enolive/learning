package de.welcz;

import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Digit {

    private final char romanDigit;
    private final boolean higherFollowerPresent;

    public Digit(String roman, int index) {
        romanDigit = roman.charAt(index);
        higherFollowerPresent = charsFrom(roman, index)
                .anyMatch(d -> Convert.romanToArabic(d) > Convert.romanToArabic(romanDigit));
    }

    private static Stream<Character> charsFrom(String roman, int index) {
        return IntStream.range(index, roman.length())
                .mapToObj(roman::charAt);
    }

    public int getArabicRepresentation() {
        int arabicValue = Convert.romanToArabic(romanDigit);
        return hasHigherFollower()
                ? -arabicValue
                : arabicValue;
    }

    public boolean hasHigherFollower() {
        return higherFollowerPresent;
    }

}
