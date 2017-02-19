using FluentAssertions.Execution;
using FluentAssertions.Primitives;
using Implementation;

namespace FluentAssertions
{
    public class BowlingGameAssertions : ObjectAssertions
    {
        private readonly BowlingGame _game;

        public BowlingGameAssertions(BowlingGame game) : base(game)
        {
            _game = game;
        }

        public void HaveTotalScoreOf(int expectedScore, string because = null, params object[] becauseArgs)
        {
            var actualScore = _game.TotalScore;
            Execute.Assertion
                .ForCondition(actualScore == expectedScore)
                .BecauseOf(because, becauseArgs)
                .FailWith("expected that the bowling score is {0}{reason}, but found {1}.", expectedScore, actualScore);
        }
    }
}