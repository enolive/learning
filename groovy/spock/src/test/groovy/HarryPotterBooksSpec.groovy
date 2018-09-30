import spock.lang.Ignore
import spock.lang.Specification
import spock.lang.Unroll

class HarryPotterBooksSpec extends Specification {

    static bookOne = new Book(8, "Philosophers Stone")
    static bookTwo = new Book(8, "two Stone")
    static bookThree = new Book(8, "three Stone")
    static bookFour = new Book(8, "four Stone")
    static bookFive = new Book(8, "five Stone")
    static allBooks = [bookOne, bookTwo, bookThree, bookFour, bookFive]

    def "a book has a specific price"() {
        given:
        def book = bookOne

        when:
        def bookPrice = book.price

        then:
        bookPrice == 8
    }

    @Unroll
    def "a set of books are different #books, #booksAreDifferentExpected"() {
        when:
        def booksAreDifferent = booksAreDifferent(books)

        then:
        booksAreDifferent == booksAreDifferentExpected

        where:
        books                         | booksAreDifferentExpected
        []                            | true
        setOfBooks(1)                 | true
        setOfBooks(2)                 | true
        setOfBooks(1) + setOfBooks(1) | false
        allBooks                      | true
        [bookOne, bookOne]            | false
        [bookOne, bookTwo]            | true
        [bookOne, bookTwo, bookOne]   | false
    }

    @Unroll
    def "a list of books contains only copies of the same book"() {
        when:
        def result = booksAreTheSame(books)

        then:
        result == booksAreTheSameExpected

        where:
        books                         | booksAreTheSameExpected
        [bookOne, bookOne, bookOne]   | true
        [bookOne, bookTwo, bookTwo]   | false
        [bookOne, bookTwo, bookThree] | false
        []                            | false
        [bookOne]                     | true
    }

    def "a set of two identical books are not discounted"() {
        given:
        def books = [bookOne, bookOne]

        when:
        def discount = getDiscount(books)

        then:
        discount == 0
    }

    @Unroll
    def "a set of books that contains copies of the same book does not receive a discount"() {
        given:
        def books = [bookOne, bookTwo, bookTwo]

        when:
        def discount = getDiscount(books)

        then:
        discount == 0
    }

    @Unroll
    def "a set of #numberOfBooks different books are discounted with #discount%"() {
        given:
        def books = setOfBooks(numberOfBooks)

        when:
        def discountResult = getDiscount(books)

        then:
        discountResult == discount

        where:
        numberOfBooks | discount
        0             | 0
        1             | 0
        2             | 5
        3             | 10
        4             | 20
        5             | 25
    }

    @Unroll
    def "applying discount of #discount% on full price of #price results in discounted price of #discountedPriceExpected"() {
        when:
        def discountedPrice = getDiscountedPrice(discount, fullPrice)

        then:
        discountedPrice == discountedPriceExpected

        where:
        discount | fullPrice | discountedPriceExpected
        0        | 10        | 10
        5        | 10        | 9.5
        10       | 10        | 9
        20       | 10        | 8
        25       | 10        | 7.5
        0        | 8         | 8
        5        | 8         | 7.6
        10       | 8         | 7.2
        20       | 8         | 6.4
        25       | 8         | 6
    }

    @Unroll
    def "the set of books #books has a full price 0f #expectedFullPrice"() {
        when:
        def fullPrice = getFullPrice(books)

        then:
        fullPrice == expectedFullPrice

        where:
        books                         | expectedFullPrice
        []                            | 0
        setOfBooks(0)                 | 0
        setOfBooks(1)                 | 8
        setOfBooks(1) + setOfBooks(1) | 16
        setOfBooks(2)                 | 16
        setOfBooks(2) + setOfBooks(1) | 24
        setOfBooks(5)                 | 40
        allBooks                      | 40
    }

    @Unroll
    def "group various books into a list of sets, each set containing different books"() {
        when:
        def listOfSetsOfBooks = organizeBooksIntoSets(books)

        then:
        listOfSetsOfBooks == expectedSetsOfBooks
        listOfSetsOfBooks.size() == sizeExpected
        listOfSetsOfBooks.size() < 1 || listOfSetsOfBooks.every { booksAreDifferent(it) }

        where:
        books                                                    | expectedSetsOfBooks                                            | sizeExpected
        []                                                       | []                                                             | 0
        setOfBooks(1)                                            | [[new Book(8, "Philosophers Stone")]]                         | 1
        setOfBooks(2)                                            | [setOfBooks(2)]                                                | 1
        setOfBooks(1) + setOfBooks(1)                            | [setOfBooks(1), setOfBooks(1)]                                 | 2
        setOfBooks(1) + setOfBooks(2)                            | [setOfBooks(2), setOfBooks(1)]                                 | 2
        [bookOne, bookOne, bookTwo, bookOne, bookThree, bookTwo] | [[bookOne, bookTwo, bookThree], [bookOne, bookTwo], [bookOne]] | 3
    }

    @Ignore
    def "a specific basket of books costs 51.2 after discount"() {
        given:
        def basketOfBooks = copiesOfBook(2, 1) + copiesOfBook(2, 2) + copiesOfBook(2, 3) + copiesOfBook(1, 4) + copiesOfBook(1, 5)

        when:
        def discountedPrice = getDiscountedPrice(basketOfBooks)

        then:
        discountedPrice == 51.2
    }

    @Unroll
    def "get copies of the same book"() {
        when:
        def books = copiesOfBook(copies, bookIndex)

        then:
        books == expectedBooks

        where:
        copies | bookIndex | expectedBooks
        0      | 0         | []
        0      | 1         | []
        1      | 1         | [bookOne]
        1      | 2         | [bookTwo]
        2      | 2         | [bookTwo, bookTwo]
    }

    def copiesOfBook(copies, bookIndex) {
        copies > 0 ? (1..copies).collect {
            allBooks.get(bookIndex - 1)
        } : []
    }

    def organizeBooksIntoSets(List books) {
        books.inject([], { result, book ->
            def foundSet = result.find { booksAreDifferent(it + [book]) } ?: []
            if (foundSet == []) result.add(foundSet)
            foundSet.add(book)
            return result
        })
    }

    def getFullPrice(books) {
        return books.inject(0, { acc, val -> acc + val.price })
    }

    def getDiscountedPrice(discount, fullPrice) {
        return fullPrice - fullPrice * discount / 100
    }

    def "four books, of which 3 are different, get a 10% discount for the set of 3, but the fourth book still costs 8 EUR"() {
        given:
        def books = setOfBooks(3) + setOfBooks(1)

        when:
        def price = getDiscountedPrice(books)

        then:
        price == 29.6
    }

    def getDiscountedPrice(books) {
        return organizeBooksIntoSets(books).collect { discountedPriceForSet(it) }.sum()
    }

    def discountedPriceForSet(setOfBooks) {
        def discount = getDiscount(setOfBooks)
        def price = getFullPrice(setOfBooks)
        return getDiscountedPrice(discount, price)
    }

    def setOfBooks(numberOfBooks) {
        return allBooks.subList(0, numberOfBooks)
    }

    def getDiscount(books) {
        def discounts = [0, 0, 5, 10, 20, 25]
        return booksAreEligibleForDiscount(books) ? discounts[books.size] : 0
    }

    def booksAreEligibleForDiscount(books) {
        return booksAreDifferent(books)
    }

    def booksAreTheSame(books) {
        return books.unique(false).size == 1
    }

    def booksAreDifferent(books) {
        return books.groupBy { it.name }.size() == books.size()
    }
}