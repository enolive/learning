import org.assertj.core.api.AbstractAssert;

class RockPaperScissorsAssert extends AbstractAssert<RockPaperScissorsAssert, Playing> {

    private RockPaperScissorsAssert(Playing playing, Class<?> selfType) {
        super(playing, selfType);
    }

    static RockPaperScissorsAssert assertThat(Playing playing) {
        return new RockPaperScissorsAssert(playing, RockPaperScissorsAssert.class);
    }

    void winsAgainst(PlayerChoice opponent) {
        Result result = actual.against(opponent);
        if (result != Result.WIN) {
            failWithMessage("expected to win with <%s> against <%s> but actually our result was <%s>.",
                    actual.getOwnChoice(), opponent, result);
        }
        Result opponentResult = Player.thatPlays(opponent).against(actual.getOwnChoice());
        if (opponentResult != Result.LOSS) {
            failWithMessage("expected that the opponent using <%s> loses against ourselves with <%s> but actually our result was <%s>",
                    opponent, actual.getOwnChoice(), opponentResult);
        }
    }
}
