import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.util.Arrays;
import java.util.Map;
import java.util.stream.Collectors;

import static com.google.common.collect.Maps.immutableEntry;

@Player.Winning(self = PlayerChoice.PAPER, opponent = PlayerChoice.ROCK)
@Player.Winning(self = PlayerChoice.ROCK, opponent = PlayerChoice.SCISSORS)
@Player.Winning(self = PlayerChoice.SCISSORS, opponent = PlayerChoice.PAPER)
public class Player {

    private Player() {
    }

    public static Playing thatPlays(PlayerChoice player) {
        return new PlayingImpl(player);
    }


    @Repeatable(Winnings.class)
    public @interface Winning {
        PlayerChoice self();

        PlayerChoice opponent();
    }


    @Retention(RetentionPolicy.RUNTIME)
    public @interface Winnings {
        @SuppressWarnings("unused") 
        Winning[] value();
    }

    private static class PlayingImpl implements Playing {
        private final PlayerChoice player;

        PlayingImpl(PlayerChoice player) {
            this.player = player;
        }

        @Override
        public Result against(PlayerChoice opponent) {
            Map<PlayerChoice, PlayerChoice> winning = determineWinningRules();
            if (getPlayer() == opponent) {
                return Result.DRAW;
            }
            if (winning.get(getPlayer()) == opponent) {
                return Result.WIN;
            }

            return Result.LOSS;
        }

        @Override
        public PlayerChoice getPlayer() {
            return player;
        }

        private Map<PlayerChoice, PlayerChoice> determineWinningRules() {
            Winning[] winnings = Player.class.getAnnotationsByType(Winning.class);
            return Arrays.stream(winnings)
                         .map(w -> immutableEntry(w.self(), w.opponent()))
                         .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        }
    }
}
