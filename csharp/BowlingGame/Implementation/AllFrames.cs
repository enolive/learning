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
                FrameType = Score == CompleteScore
                    ? FrameType.Spare : FrameType.Normal;
                FirstRoll = rolls.First();
            }

            public override bool Incomplete { get; }
            public override int Score { get; }
            public override int NumberOfRolls { get; }
            public override FrameType FrameType { get; }
            public override int FirstRoll { get; }
        }

        private sealed class StrikeFrame : Frame
        {
            public override bool Incomplete => false;
            public override int Score => CompleteScore;
            public override int NumberOfRolls => 1;
            public override FrameType FrameType => FrameType.Strike;
            public override int FirstRoll => Score;
        }

        public static IEnumerable<Frame> From(IEnumerable<int> rolls)
        {
            var rollsList = rolls.AsList();
            while (rollsList.Any())
            {
                var frame = CreateFrame(rollsList.Take(2).ToArray());
                rollsList = rollsList.Skip(frame.NumberOfRolls).AsList();
                yield return frame;
            }
        }

        private static Frame CreateFrame(IList<int> rollsList)
        {
            var firstRoll = rollsList.First();
            Frame frame;
            if (firstRoll == Frame.CompleteScore)
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