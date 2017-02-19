namespace Implementation
{
    public abstract class Frame
    {
        public const int CompleteScore = 10;

        public abstract bool Incomplete { get; }
        public abstract int Score { get; }
        public abstract int NumberOfRolls { get; }
        public abstract FrameType FrameType { get; }
        public abstract int FirstRoll { get; }
    }
}