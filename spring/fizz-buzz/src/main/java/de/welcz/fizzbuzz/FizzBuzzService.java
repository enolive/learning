package de.welcz.fizzbuzz;

import org.springframework.stereotype.Service;

@Service
public class FizzBuzzService {
    String calculate(int input) {
        if (isDivisibleBy(input, 3)) {
            return "Fizz";
        }
        if (isDivisibleBy(input, 5)) {
            return "Buzz";
        }
        return String.valueOf(input);
    }

    private boolean isDivisibleBy(int input, int divisor) {
        return input % divisor == 0;
    }
}
