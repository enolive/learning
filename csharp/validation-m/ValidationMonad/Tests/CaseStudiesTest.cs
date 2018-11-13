using System;
using FluentAssertions;
using Xunit;

namespace Tests
{
    public class CaseStudiesTest
    {
        [Theory]
        [InlineData(1, "1")]
        [InlineData(2, "2")]
        [InlineData(3, "Fizz")]
        [InlineData(6, "Fizz")]
        [InlineData(5, "Buzz")]
        [InlineData(10, "Buzz")]
        [InlineData(15, "Fizz-Buzz")]
        [InlineData(30, "Fizz-Buzz")]
        public void HappyPath(int input, string expected)
        {
            CaseStudies.Calculate(input).Should().Be(expected);
        }

        [Theory]
        [InlineData("not a number", 3)]
        [InlineData("12", 12)]
        public void RecoverFromFailureShouldWork(string input, int expected)
        {
            CaseStudies.ParseOrCountWords(input).Should().Be(expected);
        }

        [Fact]
        public void RecoveryMightFail()
        {
            Action act = () => CaseStudies.ParseOrCountWords(null);
            act.Should().Throw<ArgumentException>().WithMessage("input must not be null");
        }

        [Theory]
        [InlineData(-1000, "Input must be greater or equal zero.")]
        [InlineData(-1, "Input must be greater or equal zero.")]
        [InlineData(1000, "Input must be less than 1000.")]
        [InlineData(9999, "Input must be less than 1000.")]
        public void EdgeCases(int input, string message)
        {
            Action act = () => CaseStudies.Calculate(input);
            act.Should().Throw<ArgumentException>().WithMessage(message);
        }
    }
}