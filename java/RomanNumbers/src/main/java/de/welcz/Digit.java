package de.welcz;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Digit {
    private static final Map<Character, Integer> romanToArabic =
            new HashMap<Character, Integer>() {
                {
                    put('I', 1);
                    put('V', 5);
                    put('X', 10);
                }
            };

    private final char romanDigit;
    private final boolean higherFollowerPresent;

    public Digit(String roman, int index) {
        romanDigit = roman.charAt(index);
        higherFollowerPresent = charsFrom(roman, index)
                .anyMatch(d -> toArabic(d) > toArabic(romanDigit));
    }

    public int getArabicRepresentation() {
        int arabicValue = toArabic(romanDigit);
        return hasHigherFollower()
                ? -arabicValue
                : arabicValue;
    }

    public boolean hasHigherFollower() {
        return higherFollowerPresent;
    }

    private static Stream<Character> charsFrom(String roman, int index) {
        return IntStream.range(index, roman.length())
                .mapToObj(roman::charAt);
    }

    private static int toArabic(char romanDigit) {
        return romanToArabic.get(romanDigit);
    }
}
