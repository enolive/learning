import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

@DisplayName("Just a demo")
class JustATest {
    @DisplayName("framework should work")
    @Test
    void testSomething() {
        assertThat(true).isFalse();
    }
}
