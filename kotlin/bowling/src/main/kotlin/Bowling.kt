class Bowling {
    private var currentScore: Int = 0

    fun roll(pins: Int) {
        currentScore += pins
    }

    val score: Int
        get() = currentScore

}