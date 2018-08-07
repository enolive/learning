import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource

class RoverTest {
    @Nested
    inner class `Moving forward` {
        @ParameterizedTest
        @CsvSource(value = [
            "NORTH, 1, 0",
            "SOUTH, 1, 2",
            "EAST, 0, 1",
            "WEST, 2, 1"
        ])
        internal fun `it should move forward`(initialBearing: Bearing, expectedX: Int, expectedY: Int) {
            assertThat(Rover(Position(1, 1), initialBearing).move(Command.FORWARD))
                    .isEqualTo(Rover(Position(expectedX, expectedY), initialBearing))
        }
    }

    @Nested
    inner class `Moving backwards` {
        @Test
        internal fun `it should move to the north`() {
            assertThat(Rover(Position(1, 1), Bearing.SOUTH).move(Command.BACKWARD))
                    .isEqualTo(Rover(Position(1, 0), Bearing.SOUTH))
        }

        @Test
        internal fun `it should move to the south`() {
            assertThat(Rover(Position(1, 1), Bearing.NORTH).move(Command.BACKWARD))
                    .isEqualTo(Rover(Position(1, 2), Bearing.NORTH))
        }

        @Test
        internal fun `it should move to the east`() {
            assertThat(Rover(Position(1, 1), Bearing.WEST).move(Command.BACKWARD))
                    .isEqualTo(Rover(Position(0, 1), Bearing.WEST))
        }

        @Test
        internal fun `it should move to the west`() {
            assertThat(Rover(Position(1, 1), Bearing.EAST).move(Command.BACKWARD))
                    .isEqualTo(Rover(Position(2, 1), Bearing.EAST))
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
}

