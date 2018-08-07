data class Rover(val position: Position, val bearing: Bearing) {
    fun move(command: Command): Rover {
        return Rover(Position(1, 0), bearing)
    }

}