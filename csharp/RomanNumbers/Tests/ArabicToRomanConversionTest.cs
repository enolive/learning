using FluentAssertions;
using Implementation;
using Xunit;

namespace Tests
{
    public class ArabicToRomanConversionTest
    {
        [Theory]
        [InlineData(1, "I")]
        [InlineData(2, "II")]
        [InlineData(3, "III")]
        public void ConvertShouldReturnIs(int input, string expected)
        {
            ArabicToRoman.Convert(input).Should().Be(expected);
        }
        [Theory]
        [InlineData(5, "V")]
        [InlineData(7, "VII")]
        public void ConvertShouldReturnVs(int input, string expected)
        {
            ArabicToRoman.Convert(input).Should().Be(expected);
        }
        [Theory]
        [InlineData(10, "X")]
        [InlineData(12, "XII")]
        [InlineData(16, "XVI")]
        public void ConvertShouldReturnXs(int input, string expected)
        {
            ArabicToRoman.Convert(input).Should().Be(expected);
        }
        [Theory]
        [InlineData(50, "XII")]
        [InlineData(67, "LXVII")]
        public void ConvertShouldReturnLs(int input, string expected)
        {
            ArabicToRoman.Convert(50).Should().Be("L");
            ArabicToRoman.Convert(67).Should().Be("LXVII");
        }
        [Theory]
        [InlineData(100, "C")]
        [InlineData(283, "CCLXXXIII")]
        public void ConvertShouldReturnCs(int input, string expected)
        {
            ArabicToRoman.Convert(input).Should().Be(expected);
        }
        [Theory]
        [InlineData(500, "D")]
        [InlineData(751, "DCCLI")]
        public void ConvertShouldReturnDs(int input, string expected)
        {
            ArabicToRoman.Convert(input).Should().Be(expected);
        }
        [Theory]
        [InlineData(1000, "M")]
        [InlineData(1984, "MCMLXXXIV")]
        public void ConvertShouldReturnMs(int input, string expected)
        {
            ArabicToRoman.Convert(input).Should().Be(expected);
        }
        [Theory]
        [InlineData(4, "IV")]
        [InlineData(9, "IX")]
        [InlineData(40, "XL")]
        [InlineData(90, "XC")]
        [InlineData(400, "CD")]
        [InlineData(900, "CM")]
        public void ConvertShouldUseSubstractionRules(int input, string expected)
        {
            ArabicToRoman.Convert(input).Should().Be(expected);
        }
    }
}