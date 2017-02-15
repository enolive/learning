using Implementation;

namespace FluentAssertions
{
    public static class BowlingGameAssertionsExtension
    {
        public static BowlingGameAssertions Should(this BowlingGame game)
        {
            return new BowlingGameAssertions(game);
        }
    }
}