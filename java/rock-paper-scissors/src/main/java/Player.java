import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.util.Arrays;
import java.util.Map;
import java.util.stream.Collectors;

@SuppressWarnings("WeakerAccess")
@Player.Winning(self = PlayerChoice.PAPER, opponent = PlayerChoice.ROCK)
@Player.Winning(self = PlayerChoice.ROCK, opponent = PlayerChoice.SCISSORS)
@Player.Winning(self = PlayerChoice.SCISSORS, opponent = PlayerChoice.PAPER)
public class Player {

    private static final Map<PlayerChoice, PlayerChoice> winsAgainst = determineWinningRules();
    private static final Rule[] ruleChain = new Rule[]{
            new Rule((self, opponent) -> self == opponent, Result.DRAW),
            new Rule((self, opponent) -> winsAgainst.get(self) == opponent, Result.WIN),
            new Rule((self, opponent) -> true, Result.LOSS)};

    private Player() {
    }

    public static Playing thatPlays(PlayerChoice player) {
        return new PlayingImpl(player);
    }

    private static Map<PlayerChoice, PlayerChoice> determineWinningRules() {
        Winning[] winnings = Player.class.getAnnotationsByType(Winning.class);
        return Arrays.stream(winnings)
                     .collect(Collectors.toMap(Winning::self, Winning::opponent));
    }

    @Repeatable(Winnings.class)
    public @interface Winning {
        PlayerChoice self();

        PlayerChoice opponent();
    }


    @Retention(RetentionPolicy.RUNTIME)
    public @interface Winnings {
        @SuppressWarnings({"unused", "Required for repeatable to work"})
        Winning[] value();
    }

    private static class PlayingImpl implements Playing {
        private final PlayerChoice ownChoice;

        PlayingImpl(PlayerChoice ownChoice) {
            this.ownChoice = ownChoice;
        }

        @Override
        public Result against(PlayerChoice opponent) {
            // HINT: the rule chain is by definition always filterable due to the otherwise rule
            //noinspection ConstantConditions 
            return Arrays.stream(ruleChain)
                         .filter(r -> r.appliesTo(getOwnChoice(), opponent))
                         .map(Rule::getResult)
                         .findFirst()
                         .get();

        }

        @Override
        public PlayerChoice getOwnChoice() {
            return ownChoice;
        }
    }
}
