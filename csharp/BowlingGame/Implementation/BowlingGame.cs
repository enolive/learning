using System.Collections.Generic;
using System.Linq;

namespace Implementation
{
    public class BowlingGame
    {
        private const int AllPins = 10;
        private readonly List<int> _rolls = new List<int>();

        public int TotalScore { get; private set; }

        public void Roll(int points)
        {
            _rolls.Add(points);
            var multiplicator = 1;
            if (IsLastFrameSpare())
            {
                multiplicator = 2;
            }

            if (IsLastFrameStrike())
            {
                multiplicator = 2;
            }

            TotalScore += multiplicator * points;
        }

        public bool IsLastFrameStrike()
        {
            return IsFrameStrike(0);
        }

        private bool IsLastFrameSpare()
        {
            var scoreFromLastFrame = GetScoreFromLastFrame();
            var isSpare = IsFirstRollInFrame() && scoreFromLastFrame == AllPins;
            return isSpare;
        }

        public int GetScoreFromLastFrame()
        {
            var howManyRolls = IsFrameComplete() ? 2 : 1;
            var scoreFromLastFrame = AllRolls().Reverse().Skip(howManyRolls).Take(2).Sum();
            return scoreFromLastFrame;
        }

        private IEnumerable<int> AllRolls()
        {
            IEnumerable<int> previousRolls = _rolls;
            return previousRolls;
        }

        private bool IsFirstRollInFrame() => !IsFrameComplete();

        private bool IsFrameComplete()
        {
            var normalRolls = AllRolls().Count(points => points != AllPins);
            var isFrameComplete = normalRolls % 2 == 0;
            return isFrameComplete;
        }

        private bool IsFrameStrike(int whichFrame)
        {
            var howManyRolls = IsFrameComplete() ? 2 : 1;
            var lastThrow = AllRolls().Reverse().Skip(whichFrame).Skip(howManyRolls).Take(1).FirstOrDefault();
            if (lastThrow != AllPins)
            {
                return false;
            }
            return true;
        }
    }
}