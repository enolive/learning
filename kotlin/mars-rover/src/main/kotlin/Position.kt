data class Position(val x: Int, val y: Int) {
    private fun deltaY(delta: Int) = Position(x, y + delta)
    fun change(command: Command, bearing: Bearing) = when (command) {
        Command.FORWARD -> changeBy(1, bearing)
        Command.BACKWARD -> changeBy(-1, bearing)
        else -> this
    }

    private fun deltaX(delta: Int) = Position(x + delta, y)

    private fun changeBy(step: Int, bearing: Bearing) = when (bearing) {
        Bearing.NORTH -> deltaY(-step)
        Bearing.SOUTH -> deltaY(step)
        Bearing.EAST -> deltaX(step)
        Bearing.WEST -> deltaX(-step)
    }
}