enum Orientation {
    NORTH,
    EAST,
    SOUTH,
    WEST,
    ;

    public Orientation nextRight() {
        final var values = Orientation.values();
        final var current = ordinal();
        final var next = (current + 1) % values.length;
        return values[next];
    }
}
