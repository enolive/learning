namespace Test.Builder
{
    internal static class ASequence
    {
        public static T[] Of<T>(params T[] items)
        {
            return items;
        }
    }
}