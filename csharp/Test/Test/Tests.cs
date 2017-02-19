using System;
using FluentAssertions;
using Xunit;

namespace Test
{
    public class Tests
    {
        [Fact]
        public void SayHelloToWorld()
        {
            // arrange
            var greeter = new Greeter();
            // act
            var result = greeter.SayHello();
            // assert
            result.Should().Be("Hello World");
        }
    }
}