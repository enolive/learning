using System;
using FluentAssertions;
using Xunit;

namespace Tests
{
    public class Tests
    {
        [Fact]
        public void Test1()
        {
            true.Should().BeFalse();
        }
    }
}