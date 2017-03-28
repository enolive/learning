using System.Globalization;

namespace Implementation.Classic
{
    public class FizzBuzz
    {
        public string CalculateFor(int input)
        {
            if (input.IsDivisibleBy(3) && input.IsDivisibleBy(5))
            {
                return "Fizz-Buzz";
            }

            if (input.IsDivisibleBy(3))
            {
                return "Fizz";
            }

            if (input.IsDivisibleBy(5))
            {
                return "Buzz";
            }

            return input.ToString(CultureInfo.InvariantCulture);
        }
    }
}