using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using Implementation.Util;

namespace Implementation
{
    public class FizzBuzzEngine
    {
        private static readonly IEnumerable<Rule> Rules = ListOfRules(
            new Rule(15, "Fizz-Buzz"),
            new Rule(3, "Fizz"),
            new Rule(5, "Buzz"));

        private static IEnumerable<Rule> ListOfRules(params Rule[] rules)
        {
            return rules;
        }

        public string Calculate(int number)
        {
            return Rules
                .Where(r => number.IsDivisibleBy(r.Denominator))
                .Select(r => r.Result)
                .HeadOption()
                .GetOrElse(() => number.ToString(CultureInfo.InvariantCulture));
        }
    }
}