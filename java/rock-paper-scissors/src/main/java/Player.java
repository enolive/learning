public class Player {
    public Player plays(PlayerChoice choice) {
        return this;
    }

    public Result against(PlayerChoice choice) {
        return Result.DRAW;
    }
}
