import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

class RoverTest {
    @Test
    void defaultRover() {
        var expected = new Rover(new Position(0, 0), Orientation.NORTH);
        var rover = new Rover();
        assertThat(rover).isEqualToComparingFieldByFieldRecursively(expected);
    }

    @ParameterizedTest
    @CsvSource(value = {
            "14, 45, NORTH, 14, 44",
            "33, 13, NORTH, 33, 12",
            "5, 11, SOUTH, 5, 12",
            "22, 1, EAST, 23, 1",
            "105, 12, WEST, 104, 12",
    })
    void forward(int startX, int startY, Orientation orientation, int expectedX, int expectedY) {
        final var expectedPosition = new Position(expectedX, expectedY);
        final var startPosition = new Position(startX, startY);
        final var expected = new Rover(expectedPosition, orientation);
        final var rover = new Rover(startPosition, orientation);
        assertThat(rover.forward()).isEqualToComparingFieldByFieldRecursively(expected);
    }

    @ParameterizedTest
    @CsvSource(value = {
            "14, 45, NORTH, 14, 46",
            "33, 13, NORTH, 33, 14",
            "5, 11, SOUTH, 5, 10",
            "22, 1, EAST, 21, 1",
            "105, 12, WEST, 106, 12",
    })
    void backward(int startX, int startY, Orientation orientation, int expectedX, int expectedY) {
        final var expectedPosition = new Position(expectedX, expectedY);
        final var startPosition = new Position(startX, startY);
        final var expected = new Rover(expectedPosition, orientation);
        final var rover = new Rover(startPosition, orientation);
        assertThat(rover.backward()).isEqualToComparingFieldByFieldRecursively(expected);
    }

    @ParameterizedTest
    @CsvSource(value = {
            "NORTH, EAST",
            "EAST, SOUTH",
            "SOUTH, WEST",
            "WEST, NORTH",
    })
    void turnRight(Orientation start, Orientation end) {
        final var position = new Position(12, 33);
        final var rover = new Rover(position, start);
        final var expected = new Rover(position, end);
        assertThat(rover.turnRight()).isEqualToComparingFieldByFieldRecursively(expected);
    }

    @ParameterizedTest
    @CsvSource(value = {
            "NORTH, WEST",
            "WEST, SOUTH",
            "SOUTH, EAST",
            "EAST, NORTH",
    })
    void turnLeft(Orientation start, Orientation end) {
        final var position = new Position(12, 33);
        final var rover = new Rover(position, start);
        final var expected = new Rover(position, end);
        assertThat(rover.turnLeft()).isEqualToComparingFieldByFieldRecursively(expected);
    }
}
