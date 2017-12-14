import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class YahtzeeTest {
    @Test
    void it_should_score_ones() {
        assertThat(new Yahtzee(6, 2, 3, 4, 3).ones()).isEqualTo(0);
        assertThat(new Yahtzee(1, 2, 3, 4, 5).ones()).isEqualTo(1);
        assertThat(new Yahtzee(1, 2, 3, 4, 1).ones()).isEqualTo(2);
    }

    @Test
    void it_should_score_twos() {
        assertThat(new Yahtzee(1, 2, 3, 4, 5).twos()).isEqualTo(2);
        assertThat(new Yahtzee(2, 2, 3, 4, 5).twos()).isEqualTo(4);
        assertThat(new Yahtzee(2, 2, 2, 4, 5).twos()).isEqualTo(6);
    }

    @Test
    void it_should_score_threes() {
        assertThat(new Yahtzee(1, 2, 3, 4, 5).threes()).isEqualTo(3);
        assertThat(new Yahtzee(1, 2, 3, 4, 3).threes()).isEqualTo(6);
        assertThat(new Yahtzee(3, 3, 3, 4, 3).threes()).isEqualTo(12);
    }
    
    @Test
    void it_should_score_fours() {
        assertThat(new Yahtzee(1, 2, 3, 1, 5).fours()).isEqualTo(0);
        assertThat(new Yahtzee(1, 2, 3, 4, 5).fours()).isEqualTo(4);
        assertThat(new Yahtzee(1, 4, 3, 4, 5).fours()).isEqualTo(8);
    }
    
    @Test
    void it_should_score_fives() {
        assertThat(new Yahtzee(1, 3, 3, 4, 1).fives()).isEqualTo(0);
        assertThat(new Yahtzee(1, 5, 3, 4, 5).fives()).isEqualTo(10);
        assertThat(new Yahtzee(1, 5, 5, 4, 5).fives()).isEqualTo(15);
    }

    @Test
    void it_should_score_single_pair() {
        assertThat(new Yahtzee(1, 2, 3, 4, 5).pair()).isEqualTo(0);
        assertThat(new Yahtzee(1, 1, 3, 4, 5).pair()).isEqualTo(2);
        assertThat(new Yahtzee(1, 3, 3, 4, 5).pair()).isEqualTo(6);
        assertThat(new Yahtzee(1, 3, 3, 4, 4).pair()).isEqualTo(8);
    }

    @Test
    void it_should_score_two_pairs() {
        assertThat(new Yahtzee(4, 4, 2, 3, 5).twoPairs()).isEqualTo(0);
        assertThat(new Yahtzee(1, 1, 3, 3, 5).twoPairs()).isEqualTo(8);
        assertThat(new Yahtzee(4, 4, 3, 3, 5).twoPairs()).isEqualTo(14);
    }
}
