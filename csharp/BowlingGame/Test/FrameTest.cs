using System.Collections.Generic;
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
        }
    }

    public class AllFrames
    {
        public static IEnumerable<Frame> From(IEnumerable<int> rolls)
        {
            var frames = new List<Frame>();
            foreach (var roll in rolls)
            {
                frames.Add(new Frame(roll));
            }
            
            return frames;
        }
    }

    public class Frame
    {
        public Frame(int roll)
        {
            Incomplete = true;
        }

        public bool Incomplete { get; }
    }
}