using System.Collections.Generic;
using FluentAssertions;
using Implementation.OutsideIn;
using Moq;
using TestCommons;
using Xunit;

namespace Test
{
    public class OutsideInTest
    {
        [Theory]
        [InlineData(1, "1")]
        [InlineData(2, "2")]
        [InlineData(3, "Fizz")]
        [InlineData(5, "Buzz")]
        [InlineData(6, "Fizz")]
        [InlineData(10, "Buzz")]
        [InlineData(15, "Fizz-Buzz")]
        public void FizzBuzzShouldReturnExpectedNumbers(int input, string expectedResult)
        {
            // arrange
            var target = new FizzBuzz();
            // act
            var result = target.CalculateFor(input);
            // assert
            result.Should().Be(expectedResult);
        }

        [Fact]
        public void CalculatorShouldAskWhichRuleApplies()
        {
            // arrange
            var rule1 = ARule();
            var rule2 = ARule();
            var rules = ASequence.Of(rule1, rule2);
            var target = new FizzBuzz(rules);

            // act
            target.CalculateFor(1);

            // assert
            rule1.ToMock().Verify(x => x.AppliesTo(1), Times.Once);
            rule2.ToMock().Verify(x => x.AppliesTo(1), Times.Once);
        }

        [Fact]
        public void WhenNoRuleMatchesCalculatorShouldReturnTheNumber()
        {
            // arrange
            var target = new FizzBuzz(ASequence.Of(ARule(), ARule()));
            // act
            var result = target.CalculateFor(1);
            // assert
            result.Should().Be("1", "because no rule matches");
        }

        [Fact]
        public void CalculatorShouldReturnResultFromFirstApplyingRule()
        {
            // arrange
            var rules = ASequence.Of(AMatchingRuleWithResult("My Result"), ARule());
            var target = new FizzBuzz(rules);

            // act
            var result = target.CalculateFor(1);

            // assert
            result.Should().Be("My Result");
        }

        private static IRule AMatchingRuleWithResult(string result)
        {
            var matchingRule = ARule();
            matchingRule.ToMock().Setup(x => x.AppliesTo(It.IsAny<int>())).Returns(true);
            matchingRule.ToMock().SetupGet(x => x.Result).Returns(result);
            return matchingRule;
        }

        private static IRule ARule()
        {
            return new Mock<IRule>().Object;
        }
    }
}