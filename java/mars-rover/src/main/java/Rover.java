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
}
