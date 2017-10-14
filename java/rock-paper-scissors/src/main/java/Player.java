import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.google.common.collect.Maps.immutableEntry;

public class Player {
    private PlayerChoice player;

    public Player plays(PlayerChoice player) {
        this.player = player;
        return this;
    }

    public Result against(PlayerChoice opponent) {
        Map<PlayerChoice, PlayerChoice> winning = mapContaining(
                immutableEntry(PlayerChoice.SCISSORS, PlayerChoice.PAPER),
                immutableEntry(PlayerChoice.ROCK, PlayerChoice.SCISSORS),
                immutableEntry(PlayerChoice.PAPER, PlayerChoice.ROCK));

        if (winning.get(player) == opponent) {
            return Result.WIN;
        }

        return Result.DRAW;
    }

    private Map<PlayerChoice, PlayerChoice> mapContaining(Map.Entry<PlayerChoice, PlayerChoice>... entries) {
        return Stream.of(entries)
                     .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }


}
