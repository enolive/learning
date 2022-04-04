import io.vavr.Function1;
import io.vavr.collection.List;
import io.vavr.collection.Traversable;

import java.math.BigDecimal;

public class HarryPotter {
  public static BigDecimal calculatePrice(List<Integer> books) {
    var distinctBooks = countDistinctBooks(books);
    var bundlesGreedy = bundleGreedy(distinctBooks);
    var bundlesAdjusted = adjustBundles(bundlesGreedy);
    return (BigDecimal) bundlesAdjusted.map(HarryPotter::giveDiscount).sum();
  }

  public static List<Integer> countDistinctBooks(List<Integer> books) {
    return books.groupBy(Function1.identity())
                .mapValues(Traversable::size)
                .values()
                .sorted()
                .toList();
  }

  public static List<Integer> bundleGreedy(List<Integer> bookSets) {
    return bookSets
        .scanLeft(bookSets, (remainingBooks, __) -> removeOneBundle(remainingBooks))
        .filter(Traversable::nonEmpty)
        .map(Traversable::size);
  }

  private static List<Integer> removeOneBundle(List<Integer> remainingBooks) {
    return remainingBooks.map(it -> it - 1).filter(it -> it > 0);
  }

  public static List<Integer> adjustBundles(List<Integer> greedyBundles) {
    var howManyThrees = greedyBundles.count(it -> it == 3);
    var howManyFives = greedyBundles.count(it -> it == 5);
    var commonCount = Math.min(howManyThrees, howManyFives);
    var bundlesOfFour = List.fill(commonCount * 2, 4);
    var remainingBundlesOfThree = List.fill(howManyThrees - commonCount, 3);
    var remainingBundlesOfFive = List.fill(howManyFives - commonCount, 5);
    return greedyBundles.reject(it -> it == 3)
                        .reject(it -> it == 5)
                        .appendAll(bundlesOfFour)
                        .appendAll(remainingBundlesOfThree)
                        .appendAll(remainingBundlesOfFive);
  }

  public static BigDecimal giveDiscount(int bundleSize) {
    return new BigDecimal[]{
        BigDecimal.valueOf(8),
        BigDecimal.valueOf(15.2),
        BigDecimal.valueOf(21.6),
        BigDecimal.valueOf(25.6),
        BigDecimal.valueOf(30.0)
    }[bundleSize - 1];
  }
}

