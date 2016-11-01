package de.welcz;

import java.util.HashMap;
import java.util.Map;

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

    public Digit(String roman, int index) {
        this.romanDigit = roman.charAt(index);
    }

    public int getArabicRepresentation() {
        return romanToArabic.get(romanDigit);
    }
}
