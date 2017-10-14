public interface Playing {
    Result against(PlayerChoice opponent);

    PlayerChoice getOwnChoice();
}
