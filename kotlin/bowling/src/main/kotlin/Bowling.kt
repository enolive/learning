class Bowling {
    private val rolls = mutableListOf<Int>()
    val score: Int
        get() = allFrames()
                .fold(CurrentScore(rolls)) { accumulator, _ -> accumulator.nextFrame() }
                .points

    fun roll(pins: Int) {
        rolls.add(pins)
    }

    private fun allFrames() = (1..10)
}
