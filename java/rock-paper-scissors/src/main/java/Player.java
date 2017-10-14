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
    private PlayerChoice player;

    public Player plays(PlayerChoice player) {
        this.player = player;
        return this;
    }

    public Result against(PlayerChoice opponent) {
        Map<PlayerChoice, PlayerChoice> winning = determineWinningRules();
        if (winning.get(player) == opponent) {
            return Result.WIN;
        }

        return Result.DRAW;
    }

    private Map<PlayerChoice, PlayerChoice> determineWinningRules() {
        Winning[] winnings = getClass().getAnnotationsByType(Winning.class);
        return Arrays.stream(winnings)
                     .map(w -> immutableEntry(w.self(), w.opponent()))
                     .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    @Repeatable(Winnings.class)
    public @interface Winning {
        PlayerChoice self();

        PlayerChoice opponent();
    }


    @Retention(RetentionPolicy.RUNTIME)
    public @interface Winnings {
        Winning[] value();
    }
}
