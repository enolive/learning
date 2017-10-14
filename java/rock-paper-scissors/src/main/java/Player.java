import com.google.common.collect.Maps;

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
        
        Map<PlayerChoice, PlayerChoice> map = Stream
                .of(
                        immutableEntry(PlayerChoice.SCISSORS, PlayerChoice.PAPER),
                        immutableEntry(PlayerChoice.PAPER, PlayerChoice.ROCK)
                )
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        if (map.get(player) == opponent) {
            return Result.WIN;
        }

        return Result.DRAW;
    }


}
