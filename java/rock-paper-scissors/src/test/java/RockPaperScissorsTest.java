import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import static org.assertj.core.api.Assertions.assertThat;

class RockPaperScissorsTest {

    @ParameterizedTest
    @EnumSource(value = PlayerChoice.class)
    void sameChoiceShouldDraw(PlayerChoice choice) {
        assertThat(Player.thatPlays(choice)
                         .against(choice))
                .isEqualTo(Result.DRAW);
    }

    @Test
    void scissorsWinAgainstPaper() {
        RockPaperScissorsAssert.assertThat(Player.thatPlays(PlayerChoice.SCISSORS))
                               .winsAgainst(PlayerChoice.PAPER);
    }

    @Test
    void paperWinsAgainstRock() {
        RockPaperScissorsAssert.assertThat(Player.thatPlays(PlayerChoice.PAPER))
                               .winsAgainst(PlayerChoice.ROCK);
    }

    @Test
    void rockWinsAgainstScissors() {
        RockPaperScissorsAssert.assertThat(Player.thatPlays(PlayerChoice.ROCK))
                               .winsAgainst(PlayerChoice.SCISSORS);
    }

    @Test
    void playerShouldBeAnnotated() {
        Player.Winning[] winnings = Player.class.getAnnotationsByType(Player.Winning.class);
        assertThat(winnings).isNotEmpty();
    }
}