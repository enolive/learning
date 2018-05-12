import io.vavr.Tuple;
import io.vavr.collection.List;

public class RomanConverter {
    public String toRoman(int arabic) {
        var rules = List.of(
                Tuple.of(10, "X"),
                Tuple.of(5, "V"),
                Tuple.of(1, "I")
        );
        return rules
                .foldLeft(
                        new ArabicToRomanConversion(arabic),
                        (acc, r) -> acc.apply(r._1, r._2))
                .getResult();
    }

}
