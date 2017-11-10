import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test

class BowlingTest {
    lateinit var bowling: Bowling

    @BeforeEach
    fun setUp() {
        bowling = Bowling()
    }

    @Test
    fun `it should score a gutter game`() {
        rollMany(0, 20)
        assertThat(bowling.score).isEqualTo(0)
    }

    @Test
    fun `it should score a game with 1's`() {
        rollMany(1, 20)
        assertThat(bowling.score).isEqualTo(20)
    }

    private fun rollMany(pins: Int, times: Int) {
        (1..times).forEach { bowling.roll(pins) }
    }
}

