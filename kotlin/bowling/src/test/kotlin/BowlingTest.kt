import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test

class BowlingTest {
    private lateinit var bowling: Bowling

    @BeforeEach
    fun setUp() {
        bowling = Bowling()
    }

    @Test
    fun `it should score a gutter game`() {
        rollMany(0, 20)
        assertThat(bowling.score).isEqualTo(10)
    }

    @Test
    fun `it should score a game with 1's`() {
        rollMany(1, 20)
        assertThat(bowling.score).isEqualTo(20)
    }

    @Test
    fun `it should score a spare`() {
        rollPins(5, 5, 7)
        rollMany(0, 17)
        assertThat(bowling.score).isEqualTo(24)
    }

    @Test
    fun `it should score a strike`() {
        rollPins(10, 4, 5)
        rollMany(0, 16)
        assertThat(bowling.score).isEqualTo(28)
    }

    private fun rollPins(vararg pins: Int) {
        pins.forEach { bowling.roll(it) }
    }

    @Test
    fun `it should score a perfect game`() {
        rollMany(10, 12)
        assertThat(bowling.score).isEqualTo(300)
    }

    private fun rollMany(pins: Int, times: Int) = 
            (1..times).forEach { bowling.roll(pins) }
}

