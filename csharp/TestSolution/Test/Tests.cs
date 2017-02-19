using System;
using Xunit;
using FluentAssertions;
using Implementation;

namespace Test
{
    public class Tests
    {
        private readonly Greeter _target;

        public Tests()
        {
            _target = new Greeter();
        }

        [Fact]
        public void SayHelloShouldGreetTheWorld()
        {
            // act
            var result = _target.SayHello();

            // assert
            result.Should().Be("Hello World!");
        }

        [Theory]
        [InlineData("Christoph", "Hello Christoph!")]
        [InlineData("Julian", "Hello Julian!")]
        public void SayHelloShouldGreetThePerson(string name, string expected)
        {
            // act
            var result = _target.SayHelloTo(name);

            // assert
            result.Should().Be(expected);
        }
    }
}