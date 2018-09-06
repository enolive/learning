class Position {
    private final int x;
    private final int y;

    Position(int x, int y) {
        this.x = x;
        this.y = y;
    }

    Position nextForward(Orientation orientation) {
        return new Position(x, y - 1);
    }
}