import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class RomanToArabicTest {
    @Test
    void it_should_convert_I_to_1() {
        var converter = new RomanConverter();
        var input = "I";
        var result = converter.toArabic(input);
        assertThat(result).isEqualTo(1);
    }
}
