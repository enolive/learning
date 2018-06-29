import java.util.Objects;

class Bundle {
    private final int bookCount;
    private final int bundleCount;

    Bundle(int bookCount, int bundleCount) {
        this.bookCount = bookCount;
        this.bundleCount = bundleCount;
    }

    @Override
    public int hashCode() {
        return Objects.hash(bookCount, bundleCount);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Bundle bundle = (Bundle) o;
        return bookCount == bundle.bookCount &&
                bundleCount == bundle.bundleCount;
    }

    @Override
    public String toString() {
        return "Bundle{" +
                "bookCount=" + bookCount +
                ", bundleCount=" + bundleCount +
                '}';
    }

    int getBundleCount() {
        return bundleCount;
    }

    Bundle addBook() {
        return new Bundle(bookCount, bundleCount + 1);
    }

    boolean has(int numberOfDistinctBooks) {
        return bookCount == numberOfDistinctBooks;
    }

    boolean hasSameBookCount(Bundle bundle) {
        return bundle.bookCount == bookCount;
    }

    int getBookCount() {
        return bookCount;
    }
}
