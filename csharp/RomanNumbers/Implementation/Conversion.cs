using System.Collections.Generic;
using System.Linq;

namespace Implementation
{
    public class Conversion
    {
        private int Input { get; }
        private IEnumerable<string> Digits { get; } = Enumerable.Empty<string>();

        public Conversion(int input)
        {
            Input = input;
        }

        private Conversion(int input, IEnumerable<string> digits)
        {
            Input = input;
            Digits = digits;
        }

        public string GetResult() => string.Join(string.Empty, Digits);

        public Conversion Apply(Rule rule)
        {
            var howMany = Input / rule.Arabic;
            var digits = Digits.Concat(Enumerable.Repeat(rule.Roman, howMany));
            var input = Input % rule.Arabic;
            return new Conversion(input, digits);
        }
    }
}