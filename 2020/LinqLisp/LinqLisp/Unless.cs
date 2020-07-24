using System;
using System.Collections.Generic;
using System.Linq;

namespace LinqLisp
{
    public static partial class LinqLisp
    {
        public static IEnumerable<TSource> Unless<TSource>(this IEnumerable<TSource> source, bool cond, Func<IEnumerable<TSource>, IEnumerable<TSource>> then)
        {
            return source.When(! cond, then);
        }

        public static IOrderedEnumerable<TSource> Unless<TSource>(this IOrderedEnumerable<TSource> source, bool cond, Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TSource>> then)
        {
            return source.When(! cond, then);
        }



        public static IEnumerable<TSource> Unless<TSource>(this IEnumerable<TSource> source, Func<IEnumerable<TSource>, bool> predicate, Func<IEnumerable<TSource>, IEnumerable<TSource>> then)
        {
            return source.When(! predicate(source), then);
        }

        public static IOrderedEnumerable<TSource> Unless<TSource>(this IOrderedEnumerable<TSource> source, Func<IEnumerable<TSource>, bool> predicate, Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TSource>> then)
        {
            return source.When(! predicate(source), then);
        }



        public static IQueryable<TSource> Unless<TSource>(this IQueryable<TSource> source, bool cond, Func<IQueryable<TSource>, IQueryable<TSource>> then)
        {
            return source.When(! cond, then);
        }

        public static IOrderedQueryable<TSource> Unless<TSource>(this IOrderedQueryable<TSource> source, bool cond, Func<IOrderedQueryable<TSource>, IOrderedQueryable<TSource>> then)
        {
            return source.When(! cond, then);
        }
    }
}
