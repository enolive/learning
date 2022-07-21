import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class MainJava {
  public static <T> Stream<List<T>> tails(List<T> xs) {
    return IntStream.rangeClosed(0, xs.size() - 1)
                    .mapToObj(index -> xs.subList(index, xs.size()));
  }

  public static <T> Stream<Map.Entry<T, T>> uniquePairs(List<T> xs) {
    return tails(xs).flatMap(it -> {
      var x = it.get(0);
      var ys = it.subList(1, it.size());
      return ys.stream().map(y -> Map.entry(x, y));
    });
  }

  public static List<Map.Entry<Integer, Integer>> solve(List<Integer> xs) {
    return uniquePairs(xs).filter(entry -> entry.getKey() + entry.getValue() == 5).toList();
  }

  public static void main(String[] args) {
    var xs = List.of(1, 2, 3, 4, 5, 6, 7);
    System.out.println(solve(xs));
  }
}
