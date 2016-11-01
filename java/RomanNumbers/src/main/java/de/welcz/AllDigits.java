package de.welcz;

import java.util.Iterator;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

class AllDigits {
    static Stream<Digit> of(String roman) {
        return fromIterator(new DigitIterator(roman));
    }

    private static Stream<Digit> fromIterator(Iterator<Digit> iterator) {
        Iterable<Digit> iterable = () -> iterator;
        return StreamSupport.stream(iterable.spliterator(), false);
    }

    private static class DigitIterator implements Iterator<Digit> {
        private final String roman;
        private int position;

        DigitIterator(String roman) {
            this.roman = roman;
        }

        @Override
        public Digit next() {
            if (!hasNext()) {
                return null;
            }

            return new Digit(roman, position++);
        }

        @Override
        public boolean hasNext() {
            return position < roman.length();
        }
    }
}
