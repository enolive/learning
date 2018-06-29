import java.util.Objects;

class BookSet {
    private final int volume;
    private final int count;

    BookSet(int volume, int count) {
        this.volume = volume;
        this.count = count;
    }

    boolean notEmpty() {
        return count > 0;
    }

    int getVolume() {
        return volume;
    }

    @Override
    public int hashCode() {
        return Objects.hash(volume, count);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        BookSet bookSet = (BookSet) o;
        return volume == bookSet.volume &&
                count == bookSet.count;
    }

    @Override
    public String toString() {
        return "BookSet{" +
                "volume=" + volume +
                ", count=" + count +
                '}';
    }

    BookSet takeOneBook() {
        return new BookSet(volume, count - 1);
    }

}
