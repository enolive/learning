import io.vavr.Tuple;
import io.vavr.collection.CharSeq;
import io.vavr.collection.HashMap;

import java.text.MessageFormat;
import java.util.function.Function;

class Rover {
    private final static HashMap<Character, Function<Rover, Rover>> COMMAND_MAP =
            HashMap.ofEntries(
                    Tuple.of('f', Rover::forward),
                    Tuple.of('b', Rover::backward),
                    Tuple.of('l', Rover::turnLeft),
                    Tuple.of('r', Rover::turnRight)
            );
    private final Position position;
    private final Orientation orientation;

    Rover() {
        this(new Position(0, 0), Orientation.NORTH);
    }

    Rover(Position position, Orientation orientation) {
        this.position = position;
        this.orientation = orientation;
    }

    Rover backward() {
        return turnRight().turnRight().forward().turnRight().turnRight();
    }

    Rover turnRight() {
        return new Rover(position, orientation.nextRight());
    }

    Rover forward() {
        return new Rover(position.nextForward(orientation), orientation);
    }

    Rover turnLeft() {
        return turnRight().turnRight().turnRight();
    }

    Rover execute(String commandList) {
        return CharSeq.of(commandList)
                      .map(this::translate)
                      .foldLeft(this, this::executeAction);
    }

    private Rover executeAction(Rover current, Function<Rover, Rover> transformRover) {
        return transformRover.apply(current);
    }

    private Function<Rover, Rover> translate(char singleCommand) {
        return COMMAND_MAP.get(singleCommand)
                          .getOrElseThrow(() -> illegalCommand(singleCommand));
    }

    private IllegalArgumentException illegalCommand(Character first) {
        return new IllegalArgumentException(
                MessageFormat.format("wrong command {0}", first));
    }
}
