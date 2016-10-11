package concrete.announcements;

import core.Announce;
import core.Player;

import java.text.MessageFormat;

public class AnnounceWin extends Announce {
    public AnnounceWin(Player player1, Player player2) {
        super(player1, player2);
    }

    public String sayScore() {
        return MessageFormat.format("Win for {0}", getLeadingPlayerName());
    }
}
