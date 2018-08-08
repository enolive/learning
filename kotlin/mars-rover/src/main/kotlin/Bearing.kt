import arrow.core.andThen
import arrow.core.compose


enum class Bearing {
    NORTH,
    EAST,
    SOUTH,
    WEST;

    fun next() = next(1)

    fun prev() = next(-1)

    private fun next(step: Int) = ::nextIndex.andThen(bearingValues::get)(step)

    private fun nextIndex(step: Int): Int {
        val nextValue = (ordinal + step) % bearingValues.size
        return if (nextValue < 0) bearingValues.size + nextValue else nextValue
    }

    companion object {
        private val bearingValues = enumValues<Bearing>()
    }
}