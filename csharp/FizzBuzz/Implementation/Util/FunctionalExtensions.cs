using System.Collections.Generic;
using System.Linq;

namespace Implementation
{
    public static class FunctionalExtensions
    {
        public static Option<T> HeadOption<T>(this IEnumerable<T> sequence) where T : class
        {
            return new Option<T>(sequence.FirstOrDefault());
        }
    }
}