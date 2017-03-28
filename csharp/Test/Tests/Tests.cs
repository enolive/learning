using System;
using FluentAssertions;
using Xunit;

namespace Tests
{
    public class Tests
    {
        [Fact]
        public void OneShouldReturnOne()
        {
            var fizzBuzz = new FizzBuzz();
            var result = fizzBuzz.CalculateFor(1);
            result.Should().Be("1");
        }
    }

    public class FizzBuzz
    {
        public string CalculateFor(int number)
        {
            return "1";
        }
    }
}