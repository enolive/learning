using System.Globalization;

namespace Implementation
{
    public class FizzBuzzEngine
    {
        public string Calculate(int number)
        {
            if (IsDivisibleBy(number, 3))
            {
                return "Fizz";
            }

            return number.ToString(CultureInfo.InvariantCulture);
        }

        private static bool IsDivisibleBy(int number, int denominator)
        {
            return number % denominator == 0;
        }
    }
}