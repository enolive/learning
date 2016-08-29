package core;

public class TennisGame {

    private final Player player1;
    private final Player player2;
    private final Rules rules;

    public TennisGame(String player1Name, String player2Name) {
        player1 = new Player(player1Name);
        player2 = new Player(player2Name);
        rules = new Rules(player1, player2);
    }

    public String getScore() {
        return rules.nextAnnounce().sayScore();
    }

    public void wonPoint(String playerName) {
        whichPlayer(playerName).raiseScore();
    }

    private Player whichPlayer(String playerName) {

        return playerName.equals(player1.getName()) ? player1 : player2;
    }

}
