using FluentAssertions;
using Implementation.Classic;
using Xunit;

namespace Test
{
    public class ClassicTest
    {
        private readonly FizzBuzz _fizzBuzz;

        public ClassicTest()
        {
            _fizzBuzz = new FizzBuzz();
        }

        [Theory]
        [InlineData(1, "1")]
        [InlineData(2, "2")]
        public void NormalNumbersShouldBeReturnedAsIs(int input, string expectedResult)
        {
            // act
            var actualResult = _fizzBuzz.CalculateFor(input);
            // assert
            actualResult.Should().Be(expectedResult);
        }

        [Theory]
        [InlineData(3)]
        [InlineData(6)]
        public void NumbersDivisibleBy3ShouldReturnFizz(int input)
        {
            // act
            var actualResult = _fizzBuzz.CalculateFor(input);
            // assert
            actualResult.Should().Be("Fizz");
        }

        [Theory]
        [InlineData(5)]
        [InlineData(10)]
        public void NumbersDivisibleBy5ShouldReturnBuzz(int input)
        {
            // act
            var actualResult = _fizzBuzz.CalculateFor(input);
            // assert
            actualResult.Should().Be("Buzz");
        }

        [Theory]
        [InlineData(15)]
        [InlineData(30)]
        public void NumbersDivisibleBy3And5ShouldReturnFizzBuzz(int input)
        {
            // act
            var actualResult = _fizzBuzz.CalculateFor(input);
            // assert
            actualResult.Should().Be("Fizz-Buzz");
        }
    }
}