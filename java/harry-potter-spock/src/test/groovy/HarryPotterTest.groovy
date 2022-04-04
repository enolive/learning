import io.vavr.collection.List
import spock.lang.Specification

class HarryPotterTest extends Specification {
  def "integration test"() {
    when: "price is calculated"
    def price = HarryPotter.calculatePrice(toVavr(books))

    then: "expected price is achieved"
    price == expected

    where:
    books                    | expected
    [1, 2, 3, 4, 5]          | 30.00
    [1, 1, 2, 2, 3, 3, 4, 5] | 51.20
  }

  def "count books"() {
    when: "distinct books are counted"
    def result = HarryPotter.countDistinctBooks(toVavr(books))

    then: "expected count is returned"
    result == toVavr(expected)

    where:
    books                    | expected
    []                       | []
    [1]                      | [1]
    [1, 1]                   | [2]
    [1, 1, 1]                | [3]
    [1, 1, 1, 2, 2]          | [2, 3]
    [1, 1, 2, 2, 3, 3, 4, 5] | [1, 1, 2, 2, 2]
  }

  def "build greedy bundles"() {
    when: "count of books are bundled"
    def greedyBundles = HarryPotter.bundleGreedy(toVavr(count))

    then: "expected bundles are achieved"
    greedyBundles == toVavr(expected)

    where:
    count           | expected
    [1, 1, 2, 2, 2] | [5, 3]
    [2, 2, 2, 2, 2] | [5, 5]
    [2, 2, 2, 3, 3] | [5, 5, 2]
    [2, 2, 2]       | [3, 3]
    [1, 1, 1]       | [3]
  }

  def "adjusts bundles swapping 5,3 to 4,4"() {
    when: "greedy bundles are adjusted to achieve highest discount"
    def greedyBundles = HarryPotter.adjustBundles(toVavr(bundles))

    then: "expected bundles are achieved"
    greedyBundles == toVavr(expected)

    where:
    bundles         | expected
    []              | []
    [1, 1, 1]       | [1, 1, 1]
    [1, 1, 3]       | [1, 1, 3]
    [5, 3]          | [4, 4]
    [5, 3, 3]       | [4, 4, 3]
    [5, 3, 3, 5]    | [4, 4, 4, 4]
    [5, 3, 3, 5, 3] | [4, 4, 4, 4, 3]
  }

  def "gives discounts for sets"() {
    when: "discount is calculated"
    def result = HarryPotter.giveDiscount(bundleSize)

    then: "expected discount is returned"
    result == expected

    where:
    bundleSize | expected
    1          | 1.0 * 8
    2          | 0.95 * 2 * 8
    3          | 0.90 * 3 * 8
    4          | 0.80 * 4 * 8
    5          | 0.75 * 5 * 8
  }

  List<Integer> toVavr(java.util.List<Integer> xs) {
    List.ofAll(xs)
  }
}
