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

    @Test
    fun `it should score a spare`() {
        bowling.roll(5)
        bowling.roll(5)
        bowling.roll(7)
        rollMany(0, 17)
        assertThat(bowling.score).isEqualTo(24)
    }

    @Test
    fun `it should score a strike`() {
        bowling.roll(10)
        bowling.roll(4)
        bowling.roll(5)
        rollMany(0, 16)
        assertThat(bowling.score).isEqualTo(28)
    }

    @Test
    fun `it should score a perfect game`() {
        rollMany(10, 12)
        assertThat(bowling.score).isEqualTo(300)
    }

    private fun rollMany(pins: Int, times: Int) {
        (1..times).forEach { bowling.roll(pins) }
    }
}

