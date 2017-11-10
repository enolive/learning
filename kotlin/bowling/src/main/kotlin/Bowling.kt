class Bowling {
    private val rolls = mutableListOf<Int>()

    fun roll(pins: Int) {
        rolls.add(pins)
    }

    val score: Int
        get() {
            var rollIndex = 0
            var currentScore = 0

            (1..10).forEach {
                if (rolls[rollIndex] + rolls[rollIndex + 1] == 10) {
                    currentScore += 10 + rolls[rollIndex + 2]
                    rollIndex += 2
                } else {
                    currentScore += rolls[rollIndex] + rolls[rollIndex + 1]
                    rollIndex += 2
                }
            }
            return currentScore
        }
}