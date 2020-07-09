using System;
using System.Collections.Generic;
using System.Linq;

namespace Numerous.LinqLisp
{
    public static partial class LinqLisp
    {
        public static IEnumerable<TSource> When<TSource>(this IEnumerable<TSource> source, bool cond, Func<IEnumerable<TSource>, IEnumerable<TSource>> then)
        {
            return cond ? then(source) : source;
        }

        public static IOrderedEnumerable<TSource> When<TSource>(this IOrderedEnumerable<TSource> source, bool cond, Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TSource>> then)
        {
            return cond ? then(source) : source;
        }



        public static IEnumerable<TSource> When<TSource>(this IEnumerable<TSource> source, Func<IEnumerable<TSource>, bool> predicate, Func<IEnumerable<TSource>, IEnumerable<TSource>> then)
        {
            return predicate(source) ? then(source) : source;
        }

        public static IOrderedEnumerable<TSource> When<TSource>(this IOrderedEnumerable<TSource> source, Func<IOrderedEnumerable<TSource>, bool> predicate, Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TSource>> then)
        {
            return predicate(source) ? then(source) : source;
        }



        public static IQueryable<TSource> When<TSource>(this IQueryable<TSource> source, bool cond, Func<IQueryable<TSource>, IQueryable<TSource>> then)
        {
            return cond ? then(source) : source;
        }

        public static IOrderedQueryable<TSource> When<TSource>(this IOrderedQueryable<TSource> source, bool cond, Func<IOrderedQueryable<TSource>, IOrderedQueryable<TSource>> then)
        {
            return cond ? then(source) : source;
        }
    }
}
