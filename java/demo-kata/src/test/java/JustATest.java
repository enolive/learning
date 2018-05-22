import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.assertj.core.api.Assertions.assertThat;

@DisplayName("Just a demo")
class JustATest {

    @DisplayName("framework should work")
    @Test
    void testSomething() {
        assertThat(true).isFalse();
    }

    @ParameterizedTest(name = "{0} -> {1}")
    @CsvSource(value = {
            "5, bar",
            "4, bla",
            "42, the answer",
    })
    void testWithParams(int input, String output) {
        assertThat(input).isEqualTo(4);
        assertThat(output).isEqualTo("bla");
    }
}
