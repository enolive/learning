package concrete.announcements;

import core.Announce;
import core.Player;

import java.text.MessageFormat;

public class AnnounceNormal extends Announce {
    public AnnounceNormal(Player player1, Player player2) {
        super(player1, player2);
    }

    @Override
    public String sayScore() {
        return MessageFormat.format("{0}-{1}", sayScoreFor(this.player1), sayScoreFor(this.player2));
    }

}
