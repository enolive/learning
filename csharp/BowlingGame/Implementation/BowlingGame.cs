using System.Collections.Generic;
using System.Linq;

namespace Implementation
{
    public class BowlingGame
    {
        private const int AllPins = 10;
        private readonly List<int> _rolls = new List<int>();

        public int TotalScore
        {
            get
            {
                var frames = AllFrames.From(_rolls);
                var totalScore = 0;
                var lastFrameWas = FrameType.Normal;
                foreach (var frame in frames)
                {
                    if (lastFrameWas == FrameType.Strike)
                    {
                        totalScore += frame.Score * 2;
                    }
                    else if (lastFrameWas == FrameType.Spare)
                    {
                        totalScore += frame.FirstRoll * 2 + frame.Score - frame.FirstRoll;
                    }
                    else
                    {
                        totalScore += frame.Score;
                    }
                    lastFrameWas = frame.FrameType;
                }
                return totalScore;
            }
        }

        public void Roll(int points)
        {
            _rolls.Add(points);
        }
    }
}