class CurrentScore(val points: Int, val rollIndex: Int, val rolls: List<Int>) {
    fun next(): CurrentScore {
        return when {
            isStrike() -> scoreStrike()
            isSpare() -> scoreSpare()
            else -> scoreNormalFrame()
        }
    }

    private fun isStrike() = rolls[rollIndex] == 10

    private fun scoreStrike(): CurrentScore {
        val nextScore = points + strikePoints()
        return CurrentScore(nextScore, rollIndex + 1, rolls)
    }

    private fun strikePoints() =
            (rolls[rollIndex + 1] + rolls[rollIndex + 2] + 10)

    private fun isSpare() =
            rolls[rollIndex] + rolls[rollIndex + 1] == 10

    private fun scoreNormalFrame(): CurrentScore {
        val nextScore = points + normalPoints()
        return CurrentScore(nextScore, rollIndex + 2, rolls)
    }

    private fun scoreSpare(): CurrentScore {
        val nextScore = points + sparePoints()
        return CurrentScore(nextScore, rollIndex + 2, rolls)
    }

    private fun normalPoints() =
            rolls[rollIndex] + rolls[rollIndex + 1]

    private fun sparePoints() = 10 + rolls[rollIndex + 2]


}