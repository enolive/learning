package de.welcz;

import java.util.HashMap;
import java.util.Map;

public class Convert {
    private static final Map<Character, Integer> romanToArabic =
            new HashMap<Character, Integer>() {
                {
                    put('I', 1);
                    put('V', 5);
                    put('X', 10);
                    put('L', 50);
                    put('C', 100);
                    put('D', 500);
                    put('M', 1000);
                }
            };

    public static int romanToArabic(char romanDigit) {
        throwOnInvalidRomanDigit(romanDigit);
        return romanToArabic.get(romanDigit);
    }

    private static void throwOnInvalidRomanDigit(char romanDigit) {
        if (!romanToArabic.containsKey(romanDigit)) {
            throw new IllegalArgumentException(String.format("Character '%s' represents no valid roman number", romanDigit));
        }
    }
}
