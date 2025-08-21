using System.Collections.Generic;
using System.Linq;

namespace LinqLisp
{
    public static partial class More
    {
        public static bool None<TSource>(this IEnumerable<TSource> source)
        {
            if (source == null) return true;
            return ! source.Any();
        }
    }
}
