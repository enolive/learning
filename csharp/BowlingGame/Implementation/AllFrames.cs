using System.Collections.Generic;
using System.Linq;

namespace Implementation
{
    public class AllFrames
    {
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