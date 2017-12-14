import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class YahtzeeTest {
    @Test
    void it_should_score_ones() {
        assertThat(new Yahtzee(1, 2, 3, 4, 5).ones()).isEqualTo(1);
        assertThat(new Yahtzee(1, 2, 3, 4, 1).ones()).isEqualTo(2);
    }
}
