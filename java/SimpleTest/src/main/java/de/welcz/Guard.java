package de.welcz;

class Guard {
    static void stringIsNotNullOrEmpty(String argumentValue, String argumentName) {
        if (argumentValue == null || argumentValue.isEmpty()) {
            throw new IllegalArgumentException(argumentName + " must not be null or empty");
        }
    }
}
