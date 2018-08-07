data class Rover(val position: Position, val bearing: Bearing) {
    fun move(command: Command) = Rover(changePosition(), bearing)

    fun changePosition() = when (bearing) {
        Bearing.NORTH -> position.deltaY(-1)
        Bearing.SOUTH -> position.deltaY(1)
        Bearing.EAST -> position.deltaX(-1)
        Bearing.WEST -> position.deltaX(1)
    }
}