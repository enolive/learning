package de.welcz.fizzbuzz;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@AllArgsConstructor
class DivByRule {
    private final int divisor;
    @Getter
    private final String result;

    boolean appliesTo(int input) {
        return input % divisor == 0;
    }
}
