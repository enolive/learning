namespace Implementation
{
    public class Rule
    {
        public int Arabic { get; }
        public string Roman { get; }

        public Rule(int arabic, string roman)
        {
            Arabic = arabic;
            Roman = roman;
        }
    }
}