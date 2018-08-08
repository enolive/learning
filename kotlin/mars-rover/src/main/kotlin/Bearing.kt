import arrow.core.andThen


enum class Bearing {
    NORTH,
    EAST,
    SOUTH,
    WEST;

    fun next() = next(1)

    fun prev() = next(-1)

    private fun next(step: Int) = ::nextIndex.andThen(bearingValues::get)(step)

    private fun nextIndex(step: Int) = step.modulo(bearingValues.size)

    private fun Int.modulo(divisor: Int) = (ordinal + this).rem(divisor).toMod(divisor)

    private fun Int.toMod(divisor: Int) = if (this < 0) divisor + this else this

    companion object {
        private val bearingValues = enumValues<Bearing>()
    }
}