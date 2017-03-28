namespace Implementation.OutsideIn
{
    public class Rule : IRule
    {
        private readonly int _denominator;

        public Rule(int denominator, string result)
        {
            Result = result;
            _denominator = denominator;
        }

        public bool AppliesTo(int number)
        {
            return number % _denominator == 0;
        }

        public string Result { get; }
    }
}