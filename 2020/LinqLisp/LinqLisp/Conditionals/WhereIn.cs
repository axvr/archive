using System.Collections.Generic;
using System.Linq;

namespace LinqLisp
{
    public static partial class Conditionals
    {
        // TODO: test.
        // TODO: ensure this works with EF.
        public static IEnumerable<TSource> WhereIn<TSource>(this IEnumerable<TSource> source, IEnumerable<TSource> enumeration)
        {
            var l = enumeration?.ToHashSet();
            var c = l?.Count ?? 0;

            if (c == 1)
            {
                var i = l.First();
                return source.Where(s => s == i);
            }
            else if (c > 1)
            {
                return source.Where(s => l.Contains(s));
            }
            else
            {
                return source;
            }
        }


        // TODO: overloads.

        // TODO: overloads to select property.
    }
}
