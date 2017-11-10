class Bowling {
    private val rolls = mutableListOf<Int>()

    fun roll(pins: Int) {
        rolls.add(pins)
    }

    val score: Int
        get() {
            var current = CurrentScore(0, 0, rolls)

            allFrames().forEach {
                if (isStrike(current)) {
                    current = scoreStrike(current)
                } else if (isSpare(current)) {
                    current = scoreSpare(current)
                } else {
                    current = scoreNormalFrame(current)
                }
            }
            return current.points
        }

    private fun isStrike(current: CurrentScore) = current.rolls[current.rollIndex] == 10

    private fun scoreStrike(current: CurrentScore): CurrentScore {
        val nextScore = current.points + strikePoints(current)
        return CurrentScore(nextScore, current.rollIndex + 1, current.rolls)
    }

    private fun strikePoints(current: CurrentScore) =
            (current.rolls[current.rollIndex + 1] + current.rolls[current.rollIndex + 2] + 10)

    private fun isSpare(current: CurrentScore) =
            current.rolls[current.rollIndex] + current.rolls[current.rollIndex + 1] == 10

    private fun scoreNormalFrame(current: CurrentScore): CurrentScore {
        val nextScore = current.points + normalPoints(current)
        return CurrentScore(nextScore, current.rollIndex + 2, current.rolls)
    }

    private fun scoreSpare(current: CurrentScore): CurrentScore {
        val nextScore = current.points + sparePoints(current)
        return CurrentScore(nextScore, current.rollIndex + 2, current.rolls)
    }

    private fun normalPoints(current: CurrentScore) =
            current.rolls[current.rollIndex] + current.rolls[current.rollIndex + 1]

    private fun sparePoints(current: CurrentScore) = 10 + current.rolls[current.rollIndex + 2]

    private fun allFrames() = (1..10)
    class CurrentScore(val points: Int, val rollIndex: Int, val rolls: List<Int>) {

    }

}
