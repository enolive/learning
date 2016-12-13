namespace Implementation.Util
{
    public static class NumberExtension
    {
        public static bool IsDivisibleBy(this int number, int denominator)
        {
            return number != 0 && number % denominator == 0;
        }
    }
}