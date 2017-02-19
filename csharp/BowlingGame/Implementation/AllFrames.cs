using System.Collections.Generic;
using System.Linq;

namespace Implementation
{
    public class AllFrames
    {
        private sealed class NormalFrame : Frame
        {
            public NormalFrame(params int[] rolls)
            {
                Score = rolls.Sum();
                NumberOfRolls = rolls.Length;
                Incomplete = NumberOfRolls < 2;
            }

            public override bool Incomplete { get; }
            public override int Score { get; }
            public override int NumberOfRolls { get; }
            public override bool Strike => false;
        }

        private sealed class StrikeFrame : Frame
        {
            public override bool Incomplete => false;
            public override int Score => Frame.StrikeScore;
            public override int NumberOfRolls => 1;
            public override bool Strike => true;
        }

        public static IEnumerable<Frame> From(IEnumerable<int> rolls)
        {
            var frames = new List<Frame>();
            var rollsList = rolls;
            while (rollsList.Any())
            {
                var frame = CreateFrame(rollsList.Take(2));
                frames.Add(frame);
                rollsList = rollsList.Skip(frame.NumberOfRolls);
            }
            return frames;
        }

        private static Frame CreateFrame(IEnumerable<int> rollsList)
        {
            var firstRoll = rollsList.First();
            Frame frame;
            if (firstRoll == Frame.StrikeScore)
            {
                frame = new StrikeFrame();
            }
            else
            {
                frame = new NormalFrame(rollsList.ToArray());
            }
            return frame;
        }
    }
}