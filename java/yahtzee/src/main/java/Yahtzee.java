import java.util.Arrays;

public class Yahtzee {
    private final int[] dices;

    public Yahtzee(int... dices) {

        this.dices = dices;
    }

    public int ones() {
        return countEyesFor(1);
    }

    public int twos() {
        return countEyesFor(2);
    }

    public int threes() {
        return countEyesFor(3);
    }

    private int countEyesFor(int whichEye) {
        return (int) Arrays
                .stream(dices).filter(d -> d == whichEye)
                .count() * whichEye;
    }
}
