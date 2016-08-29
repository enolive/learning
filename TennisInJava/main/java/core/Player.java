package core;

public class Player {
    private final String name;
    private int score;

    Player(String name) {

        this.name = name;
    }

    public int getScore() {
        return score;
    }

    void raiseScore() {
        score++;
    }

    String getName() {
        return name;
    }
}
