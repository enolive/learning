package core;

import concrete.announcements.AnnounceAdvantage;
import concrete.announcements.AnnounceNormal;
import concrete.announcements.AnnounceTie;
import concrete.announcements.AnnounceWin;

class AnnounceBuilder {
    private final Player player1;
    private final Player player2;

    AnnounceBuilder(Player player1, Player player2) {
        this.player1 = player1;
        this.player2 = player2;
    }

    Announce tie() {
        return new AnnounceTie(player1, player2);
    }

    Announce win() {
        return new AnnounceWin(player1, player2);
    }

    Announce normal() {
        return new AnnounceNormal(player1, player2);
    }

    Announce advantage() {
        return new AnnounceAdvantage(player1, player2);
    }
}
