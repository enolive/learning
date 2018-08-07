enum class Bearing {
    NORTH,
    EAST,
    SOUTH,
    WEST;

    fun change(command: Command) = when (command) {
        Command.TURN_LEFT -> next(-1)
        Command.TURN_RIGHT -> next(1)
        else -> this
    }

    private fun next(step: Int): Bearing {
        val values = enumValues<Bearing>()
        val modulo = step.modulo(values.size)
        return values[modulo]
    }

    private fun Int.modulo(divisor: Int): Int {
        val nextValue = (ordinal + this).rem(divisor)
        return if (nextValue < 0) divisor + nextValue else nextValue
    }
}