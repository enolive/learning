import java.util.function.BiFunction;

class Rule {
    private final BiFunction<PlayerChoice, PlayerChoice, Boolean> applies;
    private final Result result;

    Rule(BiFunction<PlayerChoice, PlayerChoice, Boolean> applies, Result result) {
        this.applies = applies;
        this.result = result;
    }

    boolean appliesTo(PlayerChoice self, PlayerChoice opponent) {
        return applies.apply(self, opponent);
    }

    Result getResult() {
        return result;
    }
}
