using System.Linq;

namespace Implementation
{
    public static class ArabicToRoman
    {
        private static readonly Rule[] Rules =
        {
            new Rule(1000, "M"),
            new Rule(900, "CM"),
            new Rule(500, "D"),
            new Rule(400, "CD"),
            new Rule(100, "C"),
            new Rule(90, "XC"),
            new Rule(50, "L"),
            new Rule(40, "XL"),
            new Rule(10, "X"),
            new Rule(9, "IX"),
            new Rule(5, "V"),
            new Rule(4, "IV"),
            new Rule(1, "I")
        };

        public static string Convert(int input)
        {
            return Rules.Aggregate(
                    new Conversion(input),
                    (accumulator, rule) => accumulator.Apply(rule))
                .GetResult();
        }
    }
}