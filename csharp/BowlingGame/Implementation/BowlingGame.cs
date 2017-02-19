using System;
using System.Collections.Generic;

namespace Implementation
{
    public class BowlingGame
    {
        private readonly List<int> _rolls = new List<int>();

        public int TotalScore
        {
            get
            {
                var totalScore = 0;
                var multiplicator = Multiplicator.Normal;
                foreach (var frame in AllFrames.From(_rolls))
                {
                    totalScore += CalculateScoreFor(frame, multiplicator);
                    multiplicator = NewMultiplicator(frame, multiplicator);
                }
                return totalScore;
            }
        }

        public static int CalculateScoreFor(Frame frame, Multiplicator multiplicator)
        {
            int score;
            if (multiplicator == Multiplicator.Strike)
            {
                score = frame.Score * 2;
            }
            else if (multiplicator == Multiplicator.MultipleStrikes)
            {
                score = frame.Score * 3;
            }
            else if (multiplicator == Multiplicator.Spare)
            {
                score = frame.FirstRoll * 2 + frame.Score - frame.FirstRoll;
            }
            else
            {
                score = frame.Score;
            }
            return score;
        }

        private static Multiplicator NewMultiplicator(Frame frame, Multiplicator lastMultiplicator)
        {
            switch (frame.FrameType)
            {
                case FrameType.Normal:
                    return Multiplicator.Normal;
                case FrameType.Spare:
                    return Multiplicator.Spare;
                case FrameType.Strike:
                    if (lastMultiplicator == Multiplicator.Strike ||
                        lastMultiplicator == Multiplicator.MultipleStrikes)
                    {
                        return Multiplicator.MultipleStrikes;
                    }
                    return Multiplicator.Strike;
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        public void Roll(int points)
        {
            _rolls.Add(points);
        }
    }
}