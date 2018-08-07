import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource

class RoverTest {
    @Nested
    inner class `Moving forward & backward` {
        @ParameterizedTest
        @CsvSource(value = [
            "NORTH, 1, 0",
            "SOUTH, 1, 2",
            "EAST, 2, 1",
            "WEST, 0, 1"
        ])
        internal fun `it should move forward`(initialBearing: Bearing, expectedX: Int, expectedY: Int) {
            assertThat(Rover(Position(1, 1), initialBearing).move(Command.FORWARD))
                    .isEqualTo(Rover(Position(expectedX, expectedY), initialBearing))
        }

        @ParameterizedTest
        @CsvSource(value = [
            "NORTH, 1, 2",
            "SOUTH, 1, 0",
            "EAST, 0, 1",
            "WEST, 2, 1"
        ])
        internal fun `it should move backwards`(initialBearing: Bearing, expectedX: Int, expectedY: Int) {
            assertThat(Rover(Position(1, 1), initialBearing).move(Command.BACKWARD))
                    .isEqualTo(Rover(Position(expectedX, expectedY), initialBearing))
        }
    }

    @Nested
    inner class `Changing bearing` {
        @ParameterizedTest
        @CsvSource(value = [
            "NORTH, WEST",
            "WEST, SOUTH",
            "SOUTH, EAST",
            "EAST, NORTH"
        ])
        internal fun `it should turn left`(initial: Bearing, expected: Bearing) {
            assertThat(Rover(Position(1, 1), initial).move(Command.TURN_LEFT))
                    .isEqualTo(Rover(Position(1, 1), expected))
        }

        @ParameterizedTest
        @CsvSource(value = [
            "NORTH, EAST",
            "EAST, SOUTH",
            "SOUTH, WEST",
            "WEST, NORTH"
        ])
        internal fun `it should turn right`(initial: Bearing, expected: Bearing) {
            assertThat(Rover(Position(1, 1), initial).move(Command.TURN_RIGHT))
                    .isEqualTo(Rover(Position(1, 1), expected))
        }
    }

    @Test
    internal fun `it should move multiple times`() {
        assertThat(Rover(Position(1, 1), Bearing.NORTH).move("fffblffrffb"))
                .isEqualTo(Rover(Position(-1, -2), Bearing.NORTH))
    }
}

