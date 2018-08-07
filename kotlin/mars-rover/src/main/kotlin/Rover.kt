data class Rover(val position: Position, val bearing: Bearing) {
    fun move(command: Command) = Rover(changePosition(), bearing)

    fun changePosition() = when (bearing) {
        Bearing.NORTH -> Position(position.x, position.y - 1)
        Bearing.SOUTH -> Position(position.x, position.y + 1)
    }
}