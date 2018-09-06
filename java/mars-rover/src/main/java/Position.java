import io.vavr.Tuple;
import io.vavr.collection.HashMap;

import java.text.MessageFormat;
import java.util.function.Function;
import java.util.function.Supplier;

class Position {
    private final int x;
    private final int y;
    private static final HashMap<Orientation, Function<Position, Position>> NEXT_POSITION_RULES =
            HashMap.ofEntries(
                    Tuple.of(Orientation.NORTH, deltaY(-1)),
                    Tuple.of(Orientation.SOUTH, deltaY(1)),
                    Tuple.of(Orientation.EAST, deltaX(1)),
                    Tuple.of(Orientation.WEST, deltaX(-1))
            );

    Position(int x, int y) {
        this.x = x;
        this.y = y;
    }

    Position nextForward(Orientation orientation) {
        return NEXT_POSITION_RULES.get(orientation)
                                  .map(positionFunction -> positionFunction.apply(this))
                                  .getOrElseThrow(illegalOrientation(orientation));
    }

    private Supplier<IllegalArgumentException> illegalOrientation(Orientation orientation) {
        return () -> new IllegalArgumentException(
                MessageFormat.format("invalid orientation {0}", orientation));
    }

    private static Function<Position, Position> deltaX(int delta) {
        return position -> new Position(position.x + delta, position.y);
    }

    private static Function<Position, Position> deltaY(int delta) {
        return position -> new Position(position.x, position.y + delta);
    }
}