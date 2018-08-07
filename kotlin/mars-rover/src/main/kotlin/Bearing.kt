enum class Bearing {
    NORTH,
    EAST,
    SOUTH,
    WEST;

    fun next(step: Int): Bearing {
        val values = enumValues<Bearing>()
        val nextValue = (ordinal + step) % values.size
        return values[nextValue]
    }
}