import org.assertj.core.api.AbstractAssert;
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
        RockPaperScissorsAssert.assertThat(Player.thatPlays(PlayerChoice.SCISSORS)).winsAgainst(PlayerChoice.PAPER);
    }

    @Test
    void paperWinsAgainstRock() {
        RockPaperScissorsAssert.assertThat(Player.thatPlays(PlayerChoice.PAPER)).winsAgainst(PlayerChoice.ROCK);
    }

    @Test
    void rockWinsAgainstScissors() {
        RockPaperScissorsAssert.assertThat(Player.thatPlays(PlayerChoice.ROCK)).winsAgainst(PlayerChoice.SCISSORS);
    }

    @Test
    void playerShouldBeAnnotated() {
        Player.Winning[] winnings = Player.class.getAnnotationsByType(Player.Winning.class);
        assertThat(winnings).isNotEmpty();
    }
}

class RockPaperScissorsAssert extends AbstractAssert<RockPaperScissorsAssert, Playing> {

    private RockPaperScissorsAssert(Playing playing, Class<?> selfType) {
        super(playing, selfType);
    }

    static RockPaperScissorsAssert assertThat(Playing playing) {
        return new RockPaperScissorsAssert(playing, RockPaperScissorsAssert.class);
    }

    RockPaperScissorsAssert winsAgainst(PlayerChoice opponent) {
        Result result = actual.against(opponent);
        if (result != Result.WIN) {
            failWithMessage("expected to win with <%s> against <%s> but actually our result was <%s>.",
                    actual.getPlayer(), opponent, result);
        }
        Result opponentResult = Player.thatPlays(opponent).against(actual.getPlayer());
        if (opponentResult != Result.LOSS) {
            failWithMessage("expected that the opponent using <%s> loses against ourselves with <%s> but actually our result was <%s>",
                    opponent, actual.getPlayer(), opponentResult);
        }
        return this;
    }
} 
