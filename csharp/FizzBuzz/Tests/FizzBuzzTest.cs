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
        public void CalculcateShouldReturnNormalNumbersAsIs(int number, string expectedResult)
        {
            // act
            var result = _target.Calculate(number);

            // assert
            result.Should().Be(expectedResult);
        }

        [Theory]
        [InlineData(3)]
        [InlineData(6)]
        public void CalculateShouldReturnFizz(int number)
        {
            // act
            var result = _target.Calculate(number);

            // assert
            result.Should().Be("Fizz");
        }
    }
}