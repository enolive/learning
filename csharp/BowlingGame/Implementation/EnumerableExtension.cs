using System;
using System.Collections.Generic;
using System.Linq;

namespace Implementation
{
    public static class EnumerableExtension
    {
        public static void ForEach<T>(this IEnumerable<T> enumerable, Action<T> action)
        {
            foreach (var item in enumerable)
            {
                action(item);
            }
        }

        public static IList<T> AsList<T>(this IEnumerable<T> items)
        {
            var enumerable = items as IList<T> ?? items.ToList();
            return enumerable;
        }
    }
}