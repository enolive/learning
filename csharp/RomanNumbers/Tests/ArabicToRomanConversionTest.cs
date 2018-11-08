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
        [Fact]
        public void ConvertShouldReturnVs()
        {
            ArabicToRoman.Convert(5).Should().Be("V");
            ArabicToRoman.Convert(7).Should().Be("VII");
        }
        [Fact]
        public void ConvertShouldReturnXs()
        {
            ArabicToRoman.Convert(10).Should().Be("X");
            ArabicToRoman.Convert(12).Should().Be("XII");
            ArabicToRoman.Convert(16).Should().Be("XVI");
        }
        [Fact]
        public void ConvertShouldReturnLs()
        {
            ArabicToRoman.Convert(50).Should().Be("L");
            ArabicToRoman.Convert(67).Should().Be("LXVII");
        }
        [Fact]
        public void ConvertShouldReturnCs()
        {
            ArabicToRoman.Convert(100).Should().Be("C");
            ArabicToRoman.Convert(283).Should().Be("CCLXXXIII");
        }
        [Fact]
        public void ConvertShouldReturnDs()
        {
            ArabicToRoman.Convert(500).Should().Be("D");
            ArabicToRoman.Convert(751).Should().Be("DCCLI");
        }
        [Fact]
        public void ConvertShouldReturnMs()
        {
            ArabicToRoman.Convert(1000).Should().Be("M");
            ArabicToRoman.Convert(1984).Should().Be("MCMLXXXIV");
        }
        [Fact]
        public void ConvertShouldUseSubstractionRules()
        {
            ArabicToRoman.Convert(4).Should().Be("IV");
            ArabicToRoman.Convert(9).Should().Be("IX");
            ArabicToRoman.Convert(40).Should().Be("XL");
            ArabicToRoman.Convert(90).Should().Be("XC");
            ArabicToRoman.Convert(400).Should().Be("CD");
            ArabicToRoman.Convert(900).Should().Be("CM");
        }
    }
}