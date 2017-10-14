import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import static org.assertj.core.api.Assertions.assertThat;

class RockPaperScissorsTest {

    private Player player;

    @BeforeEach
    void setUp() {
        player = new Player();
    }

    @ParameterizedTest
    @EnumSource(value = PlayerChoice.class)
    void sameChoiceShouldDraw(PlayerChoice choice) {
        Result result = player.plays(choice).against(choice);
        assertThat(result).isEqualTo(Result.DRAW);
    }

    @Test
    void scissorsWinAgainstPaper() {
        Result result = player.plays(PlayerChoice.SCISSORS).against(PlayerChoice.PAPER);
        assertThat(result).isEqualTo(Result.WIN);
    }

    @Test
    void paperWinsAgainstRock() {
        Result result = player.plays(PlayerChoice.PAPER).against(PlayerChoice.ROCK);
        assertThat(result).isEqualTo(Result.WIN);
    }
}
