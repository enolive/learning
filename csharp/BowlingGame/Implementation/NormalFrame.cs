using System.Linq;

namespace Implementation
{
    internal sealed class NormalFrame : Frame
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
}