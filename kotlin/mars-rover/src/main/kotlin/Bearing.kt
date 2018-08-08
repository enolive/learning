import arrow.core.andThen

enum class Bearing {
    NORTH,
    EAST,
    SOUTH,
    WEST;

    fun next() = next(1)

    fun prev() = next(-1)

    private fun next(step: Int) = ::nextIndex.andThen(bearingValues::get)(step)

    private fun nextIndex(step: Int) = (ordinal + step).modulo(bearingValues.size)

    // HINT: kotlin only implements rem, not mod so we need to implement it by ourselves
    private fun Int.modulo(divisor: Int) = ((this % divisor) + divisor) % divisor

    companion object {
        private val bearingValues = enumValues<Bearing>()
    }
}