data class Rover(val position: Position, val bearing: Bearing) {
    fun move(command: Command) = Rover(changePosition(command), changeBearing(command))

    private fun changeBearing(command: Command) = when(command) {
        Command.TURN_LEFT -> bearing.next(-1)
        Command.TURN_RIGHT -> bearing.next(1)
        else -> bearing
    }

    fun changePosition(command: Command) = when(command) {
        Command.FORWARD -> changePositionBy(1)
        Command.BACKWARD -> changePositionBy(-1)
        else -> position
    }

    fun changePositionBy(step: Int) = when (bearing) {
        Bearing.NORTH -> position.deltaY(-step)
        Bearing.SOUTH -> position.deltaY(step)
        Bearing.EAST -> position.deltaX(-step)
        Bearing.WEST -> position.deltaX(step)
    }
}