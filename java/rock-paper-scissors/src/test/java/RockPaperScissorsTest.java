import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

public class RockPaperScissorsTest {
    @Test
    public void paperShouldDrawAgainstPaper() {
        Player player = new Player();
        Result result = player.plays(PlayerChoice.PAPER).against(PlayerChoice.PAPER);
        assertThat(result).isEqualTo(Result.DRAW);
    }
}
