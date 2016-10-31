package de.welcz.util;

public class Guard {
    public static void stringIsNotNullOrEmpty(String argumentValue, String argumentName) {
        if (argumentValue == null || argumentValue.isEmpty()) {
            throw new IllegalArgumentException(argumentName + " must not be null or empty");
        }
    }
}
