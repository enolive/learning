public class Book {
    private final int volume;

    Book(int volume) {
        this.volume = volume;
    }

    int getVolume() {
        return volume;
    }

    @Override
    public String toString() {
        return "Book{" +
                "volume=" + volume +
                '}';
    }
}
