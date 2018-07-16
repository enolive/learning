class Bundle {
    private final int numberOfDistinctBooks;
    private final int count;

    Bundle(int numberOfDistinctBooks, int count) {
        this.numberOfDistinctBooks = numberOfDistinctBooks;
        this.count = count;
    }

    boolean nonEmpty() {
        return count != 0;
    }

    int getNumberOfDistinctBooks() {
        return numberOfDistinctBooks;
    }

    int getCount() {
        return count;
    }

    @Override
    public String toString() {
        return "Bundle{" +
                "numberOfDistinctBooks=" + numberOfDistinctBooks +
                ", count=" + count +
                '}';
    }

}
