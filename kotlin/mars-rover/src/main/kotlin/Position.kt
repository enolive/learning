data class Position(val x: Int, val y: Int) {

    fun advance(bearing: Bearing) = when (bearing) {
        Bearing.NORTH -> deltaY(-1)
        Bearing.EAST -> deltaX(1)
        Bearing.SOUTH -> deltaY(1)
        Bearing.WEST -> deltaX(-1)
    }

    private fun deltaY(delta: Int) = Position(x, y + delta)
    private fun deltaX(delta: Int) = Position(x + delta, y)
}