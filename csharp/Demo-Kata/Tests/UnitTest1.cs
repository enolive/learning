using FluentAssertions;
using Xunit;

namespace Tests
{
    public class UnitTest1
    {
        [Fact]
        public void FrameworkShouldWork()
        {
            true.Should().BeTrue();
        }
    }
}