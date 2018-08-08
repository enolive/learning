import arrow.syntax.function.andThen
import arrow.syntax.function.pipe

data class Rover(val position: Position, val bearing: Bearing) {
    fun move(commands: String) = commands.map(this::translate).fold(this, Rover::move)

    fun move(command: Command) = when (command) {
        Command.FORWARD -> forward()
        Command.BACKWARD -> backward()
        Command.TURN_LEFT -> turnLeft()
        Command.TURN_RIGHT -> turnRight()
    }

    private fun forward() = Rover(position.advance(bearing), bearing)

    // HINT: why implementing backward if we can just use a chaining
    // of turning and forward to do the same?
    private fun backward() = turnLeft().turnLeft().forward().turnRight().turnRight()

    private fun turnLeft() = Rover(position, bearing.prev())

    private fun turnRight() = Rover(position, bearing.next())

    private fun translate(command: Char) = when (command) {
        'f' -> Command.FORWARD
        'b' -> Command.BACKWARD
        'l' -> Command.TURN_LEFT
        'r' -> Command.TURN_RIGHT
        else -> error("Illegal Command $command")
    }
}