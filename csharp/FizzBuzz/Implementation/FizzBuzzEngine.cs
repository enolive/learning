using System.Collections.Generic;
using System.Globalization;
using System.Linq;

namespace Implementation
{
    public class FizzBuzzEngine
    {
        public string Calculate(int number)
        {
            var rules = new List<Rule>
            {
                new Rule(15, "Fizz-Buzz"),
                new Rule(3, "Fizz"),
                new Rule(5, "Buzz"),
            };

            var matchingRule = rules
                .Where(r => IsDivisibleBy(number, r.Denominator))
                .Select(r => r.Result)
                .FirstOrDefault();
            return matchingRule ?? number.ToString(CultureInfo.InvariantCulture);
        }

        private static bool IsDivisibleBy(int number, int denominator)
        {
            return number != 0 && number % denominator == 0;
        }
    }
}