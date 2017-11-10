class CurrentScore(val points: Int, private val rollIndex: Int, private val rolls: List<Int>) {
    constructor(rolls: List<Int>) : this(0, 0, rolls)

    fun nextFrame() = when {
        isStrike() -> scoreStrike()
        isSpare() -> scoreSpare()
        else -> scoreNormalFrame()
    }

    private fun isStrike() = rolls[rollIndex] == 10

    private fun isSpare() =
            rolls[rollIndex] + rolls[rollIndex + 1] == 10

    private fun scoreStrike() = 
            CurrentScore(points + strikePoints(), rollIndex + 1, rolls)

    private fun scoreSpare() = 
            CurrentScore(points + sparePoints(), rollIndex + 2, rolls)

    private fun scoreNormalFrame() = 
            CurrentScore(points + normalPoints(), rollIndex + 2, rolls)

    private fun strikePoints(): Int = 
            10 + rolls[rollIndex + 1] + rolls[rollIndex + 2]

    private fun sparePoints() = 10 + rolls[rollIndex + 2]

    private fun normalPoints() =
            rolls[rollIndex] + rolls[rollIndex + 1]
}