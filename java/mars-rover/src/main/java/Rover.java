class Rover {
    private final Position position;
    private final Orientation orientation;

    Rover(Position position, Orientation orientation) {
        this.position = position;
        this.orientation = orientation;
    }

    Rover() {
        this(new Position(0, 0), Orientation.NORTH);
    }

    Rover forward() {
        return new Rover(position.nextForward(orientation), orientation);
    }

    Rover turnRight() {
        return new Rover(position, orientation.nextRight());
    }

    Rover backward() {
        return turnRight().turnRight().forward().turnRight().turnRight();
    }

    Rover turnLeft() {
        return turnRight().turnRight().turnRight();
    }
}
