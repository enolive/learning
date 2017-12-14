import java.util.Arrays;

public class Yahtzee {
    private final int[] dices;

    public Yahtzee(int ...dices) {

        this.dices = dices;
    }

    public int ones() {
        return (int) Arrays.stream(dices).filter(d -> d == 1).count();
    }

    public int twos() {
        return (int) Arrays.stream(dices).filter(d -> d == 2).count() * 2;
    }
}
