data class Rover(val position: Position, val bearing: Bearing) {
    fun move(command: Command) = Rover(changePosition(command), changeBearing(command))

    private fun changeBearing(command: Command) = when (command) {
        Command.TURN_LEFT -> bearing.next(-1)
        Command.TURN_RIGHT -> bearing.next(1)
        else -> bearing
    }

    fun changePosition(command: Command) = when (command) {
        Command.FORWARD -> changePositionBy(1)
        Command.BACKWARD -> changePositionBy(-1)
        else -> position
    }

    fun changePositionBy(step: Int) = when (bearing) {
        Bearing.NORTH -> position.deltaY(-step)
        Bearing.SOUTH -> position.deltaY(step)
        Bearing.EAST -> position.deltaX(step)
        Bearing.WEST -> position.deltaX(-step)
    }

    fun move(commands: String) = commands.map(this::translate).fold(this, Rover::move)

    private fun translate(c: Char): Command {
        return when (c) {
            'f' -> Command.FORWARD
            'b' -> Command.BACKWARD
            'l' -> Command.TURN_LEFT
            'r' -> Command.TURN_RIGHT
            else -> error("Illegal Command $c")
        }
    }
}