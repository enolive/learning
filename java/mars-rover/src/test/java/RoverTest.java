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
    })
    void forward(int startX, int startY, Orientation orientation, int expectedX, int expectedY) {
        var expected = new Rover(new Position(expectedX, expectedY), orientation);
        var rover = new Rover(new Position(startX, startY), orientation);
        assertThat(rover.forward()).isEqualToComparingFieldByFieldRecursively(expected);
    }
}
