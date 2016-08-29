package concrete.announcements;

import core.Announce;
import core.Player;

public class AnnounceTie extends Announce {
    public AnnounceTie(Player player1, Player player2) {
        super(player1, player2);
    }

    @Override
    public String sayScore() {
        if (player1.getScore() > 2) {
            return "Deuce";
        }

        return sayScoreFor(player1) + "-All";
    }

}
