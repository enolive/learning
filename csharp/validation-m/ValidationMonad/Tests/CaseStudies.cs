using System;
using static Tests.Prelude;

namespace Tests
{
    internal static class CaseStudies
    {
        public static string Calculate(int input) =>
            Validate(input)
                .Map(CalculateSafe)
                .GetOrElseThrow(err => new ArgumentException(err));

        private static IMaybeValid<int, string> Validate(int input) =>
            Success<int, string>(input)
                .FilterOrElse(i => i >= 0, _ => "Input must be greater or equal zero.")
                .FilterOrElse(i => i < 1000, _ => "Input must be less than 1000.");

        private static string CalculateSafe(int input)
        {
            if (input % 15 == 0)
            {
                return "Fizz-Buzz";
            }

            if (input % 3 == 0)
            {
                return "Fizz";
            }

            if (input % 5 == 0)
            {
                return "Buzz";
            }

            return input.ToString();
        }

        public static int ParseOrCountWords(string input) =>
            Success<string, string>(input)
                .FlatMap(ParseIt)
                .MapFailure(_ => input)
                .FlatMapFailure(CountWords)
                .GetOrElseThrow(err => new ArgumentException(err));

        private static IMaybeValid<int, string> CountWords(string failingInput) =>
            failingInput == null
                ? Failure<int, string>("input must not be null")
                : Success<int, string>(failingInput.Split(' ').Length);

        private static IMaybeValid<int, string> ParseIt(string arg) =>
            int.TryParse(arg, out var test)
                ? Success<int, string>(test)
                : Failure<int, string>("input is not a valid decimal number");
    }
}