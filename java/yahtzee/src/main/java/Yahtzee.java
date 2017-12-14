public class Yahtzee {
    private final int[] dices;

    public Yahtzee(int ...dices) {

        this.dices = dices;
    }

    public int ones() {
        int count = 0;
        for (int dice : dices) {
            if (dice == 1) {
                count++;
            }
        }
        return count;
    }
}
