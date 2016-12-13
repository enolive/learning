namespace Implementation.Util
{
    public class Rule
    {
        public int Denominator { get; private set; }
        public string Result { get; private set; }

        public Rule(int denominator, string result)
        {
            Denominator = denominator;
            Result = result;
        }
    }
}