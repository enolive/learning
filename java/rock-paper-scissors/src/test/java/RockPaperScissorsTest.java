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
        assertThat(player.plays(choice)
                         .against(choice))
                .isEqualTo(Result.DRAW);
    }

    @Test
    void scissorsWinAgainstPaper() {
        assertThat(player.plays(PlayerChoice.SCISSORS)
                         .against(PlayerChoice.PAPER))
                .isEqualTo(Result.WIN);
    }

    @Test
    void paperWinsAgainstRock() {
        assertThat(player.plays(PlayerChoice.PAPER)
                         .against(PlayerChoice.ROCK))
                .isEqualTo(Result.WIN);
    }

    @Test
    void rockWinsAgainstScissors() {
        assertThat(player.plays(PlayerChoice.ROCK)
                         .against(PlayerChoice.SCISSORS))
                .isEqualTo(Result.WIN);
        assertThat(player.plays(PlayerChoice.SCISSORS)
                         .against(PlayerChoice.ROCK))
                .isEqualTo(Result.LOSS);
    }

    @Test
    void playerShouldBeAnnotated() {
        Player.Winning[] winnings = player.getClass().getAnnotationsByType(Player.Winning.class);
        assertThat(winnings).isNotEmpty();
    }
}
