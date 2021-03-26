using System;
using System.Collections.Generic;
using FluentAssertions;
using FluentAssertions.Extensions;
using Xunit;

namespace DateTimeIso
{
    public class IsoDate
    {
        public static IEnumerable<object[]> GetDateTimes()
        {
            yield return new object[] {1.January(2020), "2020-01-01"};
            yield return new object[] {23.June(1978), "1978-06-23"};
            yield return new object[] {26.March(2021), "2021-03-26"};
        }

        [Theory]
        [MemberData(nameof(GetDateTimes))]
        public void Test1(DateTime input, string expected)
        {
            input.ToString("yyyy-MM-dd").Should().Be(expected);
        }
    }
}