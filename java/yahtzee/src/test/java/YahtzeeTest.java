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

    @Test
    void it_should_score_three_of_a_kind() {
        assertThat(new Yahtzee(1, 1, 3, 4, 5).threeOfAKind()).isEqualTo(0);
        assertThat(new Yahtzee(1, 1, 1, 4, 5).threeOfAKind()).isEqualTo(3);
        assertThat(new Yahtzee(3, 3, 3, 4, 5).threeOfAKind()).isEqualTo(9);
    }

    @Test
    void it_should_score_four_of_a_kind() {
        assertThat(new Yahtzee(5, 1, 1, 1, 2).fourOfAKind()).isEqualTo(0);
        assertThat(new Yahtzee(1, 1, 1, 1, 2).fourOfAKind()).isEqualTo(4);
        assertThat(new Yahtzee(1, 3, 3, 3, 3).fourOfAKind()).isEqualTo(12);
    }

    @Test
    void it_should_score_a_small_straight() {
        assertThat(new Yahtzee(1, 2, 3, 4, 6).smallStraight()).isEqualTo(0);
        assertThat(new Yahtzee(1, 2, 3, 4, 5).smallStraight()).isEqualTo(15);
        assertThat(new Yahtzee(5, 2, 3, 4, 1).smallStraight()).isEqualTo(15);
    }

    @Test
    void it_should_score_a_large_straight() {
        assertThat(new Yahtzee(2, 3, 4, 5, 1).largeStraight()).isEqualTo(0);
        assertThat(new Yahtzee(2, 3, 4, 5, 6).largeStraight()).isEqualTo(20);
        assertThat(new Yahtzee(2, 3, 6, 4, 5).largeStraight()).isEqualTo(20);
    }

    @Test
    void it_should_score_a_chance() {
        assertThat(new Yahtzee(4, 3, 4, 2, 6).chance()).isEqualTo(19);
        assertThat(new Yahtzee(1, 5, 3, 1, 6).chance()).isEqualTo(16);        
    }

    @Test
    void it_should_score_a_full_house() {
        assertThat(new Yahtzee(5, 5, 5, 5, 5).fullHouse()).isEqualTo(0);
        assertThat(new Yahtzee(3, 3, 1, 2, 2).fullHouse()).isEqualTo(0);
        assertThat(new Yahtzee(3, 3, 3, 2, 2).fullHouse()).isEqualTo(13);
        assertThat(new Yahtzee(2, 3, 3, 2, 2).fullHouse()).isEqualTo(12);
    }
    @Test
    void it_should_score_a_yahtzee() {
        assertThat(new Yahtzee(2, 2, 2, 2, 4).yahtzee()).isEqualTo(0);
        assertThat(new Yahtzee(2, 2, 2, 2, 2).yahtzee()).isEqualTo(10);
        assertThat(new Yahtzee(6, 6, 6, 6, 6).yahtzee()).isEqualTo(30);
    }
}
