import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Test

class RoverTest {
    @Test
    internal fun `it should move to the north`() {
        val rover = Rover(Position(1, 1), Bearing.NORTH)
        assertThat(rover.move(Command.FORWARD)).isEqualTo(Rover(Position(1, 0), Bearing.NORTH))
    }
}

data class Rover(val position: Position, val bearing: Bearing) {
    fun move(command: Command): Rover {
        return Rover(Position(1, 0), bearing)
    }

}

enum class Command {
    FORWARD
}

data class Position(val x: Int, val y: Int)

enum class Bearing {
    NORTH
}
