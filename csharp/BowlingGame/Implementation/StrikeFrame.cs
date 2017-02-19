namespace Implementation
{
    internal sealed class StrikeFrame : Frame
    {
        public override bool Incomplete => false;
        public override int Score => Frame.StrikeScore;
        public override int NumberOfRolls => 1;
        public override bool Strike => true;
    }
}