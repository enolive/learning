import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Test

class RoverTest {
    @Test
    internal fun `it should move to the north`() {
        assertThat(Rover(Position(1, 1), Bearing.NORTH).move(Command.FORWARD))
                .isEqualTo(Rover(Position(1, 0), Bearing.NORTH))
    }

    @Test
    internal fun `it should move to the south`() {
        assertThat(Rover(Position(1, 1), Bearing.SOUTH).move(Command.FORWARD))
                .isEqualTo(Rover(Position(1, 2), Bearing.SOUTH))
    }
}

