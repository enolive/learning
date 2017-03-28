using System.Collections.Generic;
using System.Globalization;
using System.Linq;

namespace Implementation.OutsideIn
{
    public class FizzBuzz
    {
        private readonly IEnumerable<IRule> _rules;

        public FizzBuzz(IEnumerable<IRule> rules)
        {
            _rules = rules;
        }

        public FizzBuzz() : this(StandardFizzBuzzRules())
        {
        }

        private static IEnumerable<Rule> StandardFizzBuzzRules()
        {
            return new []
            {
                new Rule(15, "Fizz-Buzz"),
                new Rule(3, "Fizz"),
                new Rule(5, "Buzz"),
            };
        }

        public string CalculateFor(int number)
        {
            var result = _rules
                .Where(r => r.AppliesTo(number))
                .Select(r => r.Result)
                .FirstOrDefault();
            return result ?? number.ToString(CultureInfo.InvariantCulture);
        }
    }
}