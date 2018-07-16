class BookSet {
    private final int volume;
    private final int count;

    BookSet(int volume, int count) {
        this.volume = volume;
        this.count = count;
    }

    int getVolume() {
        return volume;
    }

    int getCount() {
        return count;
    }

    @Override
    public String toString() {
        return "BookSet{" +
                "volume=" + volume +
                ", count=" + count +
                '}';
    }
}
