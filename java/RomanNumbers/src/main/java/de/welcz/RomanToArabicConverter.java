package de.welcz;

public class RomanToArabicConverter {
    public int toArabic(String roman) {
        return AllDigits.of(roman)
                .mapToInt(Digit::getArabicRepresentation)
                .sum();
    }
}
