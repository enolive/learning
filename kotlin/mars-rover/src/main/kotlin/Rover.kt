data class Rover(val position: Position, val bearing: Bearing) {
    fun move(commands: String) = commands.map(this::translate).fold(this, Rover::move)

    fun move(command: Command) = Rover(position.change(command, bearing), bearing.change(command))

    private fun translate(c: Char): Command {
        return when (c) {
            'f' -> Command.FORWARD
            'b' -> Command.BACKWARD
            'l' -> Command.TURN_LEFT
            'r' -> Command.TURN_RIGHT
            else -> error("Illegal Command $c")
        }
    }
}