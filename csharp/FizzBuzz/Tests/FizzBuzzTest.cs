using FluentAssertions;
using Implementation;
using Xunit;

namespace Tests
{
    public class FizzBuzzTest
    {
        private readonly FizzBuzzEngine _target;

        public FizzBuzzTest()
        {
            _target = new FizzBuzzEngine();
        }

        [Theory]
        [InlineData(0, "0")]
        [InlineData(1, "1")]
        public void NormalNumberShouldReturnTheNumberAsString(int number, string expectedResult)
        {
            // act
            var result = _target.Calculate(number);

            // assert
            result.Should().Be(expectedResult);
        }

        [Theory]
        [InlineData(3)]
        [InlineData(6)]
        public void NumbersDivisibleBy3ShouldReturnFizz(int number)
        {
            // act
            var result = _target.Calculate(number);

            // assert
            result.Should().Be("Fizz");
        }

        [Theory]
        [InlineData(5)]
        [InlineData(10)]
        public void NumbersDivisibleBy5ShouldReturnBuzz(int number)
        {
            // act
            var result = _target.Calculate(number);

            // assert
            result.Should().Be("Buzz");
        }

        [Theory]
        [InlineData(15)]
        [InlineData(30)]
        public void NumbersDivisibleBy3And5ShouldReturnFizzBuzz(int number)
        {
            // act
            var result = _target.Calculate(number);

            // assert
            result.Should().Be("Fizz-Buzz");
        }
    }
}