namespace Implementation.Classic
{
    internal static class NumberExtension
    {
        public static bool IsDivisibleBy(this int input, int determinator)
        {
            return input % determinator == 0;
        }
    }
}