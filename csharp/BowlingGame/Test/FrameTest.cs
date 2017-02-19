using System.Linq;
using FluentAssertions;
using Implementation;
using Test.Builder;
using Xunit;

namespace Test
{
    public class FrameTest
    {
        [Fact]
        public void EmptyRollsShouldResultInNoFrames()
        {
            // arrange
            var emptyRolls = Enumerable.Empty<int>();
            // act
            var frames = AllFrames.From(emptyRolls);
            // assert
            frames.Should().BeEmpty();
        }

        [Fact]
        public void OneRollShouldResultInOneIncompleteFrame()
        {
            // arrange
            var rolls = ASequence.Of(1);
            // act
            var frames = AllFrames.From(rolls).AsList();
            // assert
            frames.Should().HaveCount(1);
            var firstFrame = frames.First();
            firstFrame.Incomplete.Should().BeTrue();
            firstFrame.Score.Should().Be(1);
            firstFrame.FrameType.Should().Be(FrameType.Normal);
        }

        [Fact]
        public void TwoRollsWithScoreOfTenShouldBeSpare()
        {
            // arrange
            var rolls = ASequence.Of(4, 6);
            // act
            var frame = AllFrames.From(rolls).First();
            // assert
            frame.Incomplete.Should().BeFalse();
            frame.Score.Should().Be(10);
            frame.FrameType.Should().Be(FrameType.Spare);
        }

        [Fact]
        public void TwoRollsShouldResultInOneCompleteFrame()
        {
            // arrange
            var rolls = ASequence.Of(1, 2);
            // act
            var frames = AllFrames.From(rolls).AsList();
            // assert
            frames.Should().HaveCount(1);
            var firstFrame = frames.First();
            firstFrame.Incomplete.Should().BeFalse();
            firstFrame.Score.Should().Be(3);
            firstFrame.FrameType.Should().Be(FrameType.Normal);
        }

        [Fact]
        public void StrikeShouldResultInOneCompleteFrame()
        {
            // arrange
            var rolls = ASequence.Of(10, 5);
            // act
            var frame = AllFrames.From(rolls).First();
            // assert
            frame.Incomplete.Should().BeFalse();
            frame.Score.Should().Be(10);
            frame.FrameType.Should().Be(FrameType.Strike);
        }

        [Fact]
        public void MoreThanTwoRollsShouldResultIntoSeparateFrames()
        {
            // arrange
            var rolls = ASequence.Of(6, 3, 10, 4, 2);
            // act
            var frames = AllFrames.From(rolls).AsList();
            // assert
            frames.Should().HaveCount(3);
            var frameScores = frames.Select(f => f.Score);
            frameScores.Should().ContainInOrder(9, 10, 6);
        }
    }
}