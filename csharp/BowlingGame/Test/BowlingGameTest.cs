using System.Linq;
using FluentAssertions;
using Implementation;
using Xunit;

namespace Test
{
    public class BowlingGameTest
    {
        private readonly BowlingGame _target;

        public BowlingGameTest()
        {
            _target = new BowlingGame();
        }

        [Fact]
        public void StrikeShouldDoubleFollowingPointsInFrame()
        {
            // act
            RollPoints(10, 8, 1, 3);
            // assert
            _target.Should().HaveTotalScoreOf(31);
        }

        // [Fact]
        // TODO: ignored because i need to write an infrastructure for frames first
        public void DoubleStrikeShouldTripleFollowingPointsInFrame()
        {
            // act
            RollPoints(10, 10, 2, 3);
            // assert
            _target.Should().HaveTotalScoreOf(45);
        }

        [Fact]
        public void GutterGameShouldScoreZero()
        {
            // act
            RollMany(20, 0);
            // assert
            _target.Should().HaveTotalScoreOf(0);
        }

        [Fact]
        public void GameWithOnlyOneRollsShouldScoreTwenty()
        {
            // act
            RollMany(20, 1);
            // assert
            _target.Should().HaveTotalScoreOf(20);
        }

        [Fact]
        public void FirstRollAfterSpareShouldBeDoubled()
        {
            // act
            RollPoints(7, 3, 1, 3);
            // assert
            _target.Should().HaveTotalScoreOf(15, "because the first roll after a spare is doubled");
        }

        [Fact]
        public void GameShouldHonorFramesForSpareScoring()
        {
            // act
            RollPoints(2, 7, 3, 1);
            // assert
            _target.Should().HaveTotalScoreOf(13, "because we have no spare");
        }

        private void RollMany(int count, int points)
        {
            var loop = Enumerable.Repeat(points, count).ToArray();
            RollPoints(loop);
        }

        private void RollPoints(params int[] points)
        {
            points.ForEach(p => _target.Roll(p));
        }
    }
}