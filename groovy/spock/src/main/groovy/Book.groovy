class Book {
    private final String name
    public final int price

    Book(int price, String name) {
        this.price = price
        this.name = name
    }

    @Override
    String toString() {
        return "Book{" +
                "name='" + name + '\'' +
                ", price=" + price +
                '}';
    }

    boolean equals(o) {
        if (this.is(o)) return true
        if (getClass() != o.class) return false

        Book book = (Book) o

        if (price != book.price) return false
        if (name != book.name) return false

        return true
    }

    int hashCode() {
        int result
        result = (name != null ? name.hashCode() : 0)
        result = 31 * result + price
        return result
    }
}
