package core;

class Rules {

    private final Player player1;
    private final Player player2;

    Rules(Player player1, Player player2) {

        this.player1 = player1;
        this.player2 = player2;
    }

    Announce nextAnnounce() {
        AnnounceBuilder announce = Announce.forPlayers(player1, player2);
        if (havePlayersTied()) {
            return announce.tie();
        }

        if (havePlayersNormalScore()) {
            return announce.normal();
        }

        if (hasOnePlayerAnAdvantage()) {
            return announce.advantage();
        }

        return announce.win();
    }

    private boolean hasOnePlayerAnAdvantage() {
        return Math.abs(player1.getScore() - player2.getScore()) == 1;
    }

    private boolean havePlayersNormalScore() {
        return player1.getScore() < 4 && player2.getScore() < 4;
    }

    private boolean havePlayersTied() {
        return player1.getScore() == player2.getScore();
    }
}
