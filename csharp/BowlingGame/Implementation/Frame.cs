namespace Implementation
{
    public abstract class Frame
    {
        public const int StrikeScore = 10;

        public abstract bool Incomplete { get; }
        public abstract int Score { get; }
        public abstract int NumberOfRolls { get; }
        public abstract bool Strike { get; }
    }
}