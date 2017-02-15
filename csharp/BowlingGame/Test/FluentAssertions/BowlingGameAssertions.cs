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

        public void HaveScoreFromLastFrame(int expectedScore, string because = null, params object[] becauseArgs)
        {
            var actualScore = _game.GetScoreFromLastFrame();
            Execute.Assertion
                .ForCondition(actualScore == expectedScore)
                .BecauseOf(because, becauseArgs)
                .FailWith("expected that the last frame score is {0}{reason}, but it is actually {1}.", expectedScore,
                    actualScore);
        }

        public void HaveStrikeInLastFrame(string because = null, params object[] becauseArgs)
        {
            var isStrike = _game.IsLastFrameStrike();
            Execute.Assertion
                .ForCondition(isStrike)
                .BecauseOf(because, becauseArgs)
                .FailWith("expected that the last frame was a strike{reason}, but it isn't.");
        }

        public void NotHaveStrikeInLastFrame(string because = null, params object[] becauseArgs)
        {
            var isStrike = _game.IsLastFrameStrike();
            Execute.Assertion
                .ForCondition(!isStrike)
                .BecauseOf(because, becauseArgs)
                .FailWith("expected that the last frame was not a strike{reason}, but it is.");
        }
    }
}