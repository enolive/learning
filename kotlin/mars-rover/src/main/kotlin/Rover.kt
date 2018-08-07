data class Rover(val position: Position, val bearing: Bearing) {
    fun move(command: Command) = Rover(changePosition(command), bearing)

    fun changePosition(command: Command) = when(command) {
        Command.FORWARD -> changePositionBy(1)
        Command.BACKWARD -> changePositionBy(-1)
    }

    fun changePositionBy(step: Int) = when (bearing) {
        Bearing.NORTH -> position.deltaY(-step)
        Bearing.SOUTH -> position.deltaY(step)
        Bearing.EAST -> position.deltaX(-step)
        Bearing.WEST -> position.deltaX(step)
    }
}