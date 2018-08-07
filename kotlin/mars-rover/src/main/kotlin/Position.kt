data class Position(val x: Int, val y: Int) {
    fun deltaY(delta: Int) = Position(x, y + delta)
}