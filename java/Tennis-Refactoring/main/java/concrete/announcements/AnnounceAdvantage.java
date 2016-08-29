package concrete.announcements;

import core.Announce;
import core.Player;

import java.text.MessageFormat;


public class AnnounceAdvantage extends Announce {
    public AnnounceAdvantage(Player player1, Player player2) {
        super(player1, player2);
    }

    @Override
    public String sayScore() {
        return MessageFormat.format("Advantage {0}", getLeadingPlayerName());
    }
}
