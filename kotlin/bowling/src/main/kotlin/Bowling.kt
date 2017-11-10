class Bowling {
    private val rolls = mutableListOf<Int>()

    fun roll(pins: Int) {
        rolls.add(pins)
    }

    val score: Int
        get() {
            var current = CurrentScore(0, 0, rolls)

            allFrames().forEach {
                current = current.next()
            }
            return current.points
        }

    private fun allFrames() = (1..10)

}
