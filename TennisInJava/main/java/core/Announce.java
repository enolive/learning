package core;

import java.util.HashMap;
import java.util.Map;

public abstract class Announce {

    protected final Player player1;
    protected final Player player2;

    protected Announce(Player player1, Player player2) {
        this.player1 = player1;
        this.player2 = player2;
    }

    static AnnounceBuilder forPlayers(Player player1, Player player2) {
        return new AnnounceBuilder(player1, player2);
    }

    protected static String sayScoreFor(Player player) {
        int score11 = player.getScore();
        Map<Integer, String> scoreMap = new HashMap<>();
        scoreMap.put(0, "Love");
        scoreMap.put(1, "Fifteen");
        scoreMap.put(2, "Thirty");
        scoreMap.put(3, "Forty");
        return scoreMap.get(score11);
    }

    protected String getLeadingPlayerName() {
        return this.player2.getScore() > this.player1.getScore() ? this.player2.getName() : this.player1.getName();
    }

    public abstract String sayScore();
}
