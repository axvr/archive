using System;
using System.Collections.Generic;
using System.Linq;

namespace LinqLisp
{
    public static partial class Conditionals
    {
        public static IEnumerable<TResult> If<TSource, TResult>(
                this IEnumerable<TSource> source,
                bool cond,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> @else)
        {
            return cond ? then(source) : @else(source);
        }

        public static IOrderedEnumerable<TResult> If<TSource, TResult>(
                this IEnumerable<TSource> source,
                bool cond,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return cond ? then(source) : @else(source);
        }

        public static IOrderedEnumerable<TResult> If<TSource, TResult>(
                this IOrderedEnumerable<TSource> source,
                bool cond,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return cond ? then(source) : @else(source);
        }



        public static TResult If<TSource, TResult>(
                this IEnumerable<TSource> source,
                bool cond,
                Func<IEnumerable<TSource>, TResult> then,
                Func<IEnumerable<TSource>, TResult> @else)
        {
            return cond ? then(source) : @else(source);
        }

        public static TResult If<TSource, TResult>(
                this IOrderedEnumerable<TSource> source,
                bool cond,
                Func<IOrderedEnumerable<TSource>, TResult> then,
                Func<IOrderedEnumerable<TSource>, TResult> @else)
        {
            return cond ? then(source) : @else(source);
        }



        public static IEnumerable<TResult> If<TSource, TResult>(
                this IEnumerable<TSource> source,
                Func<IEnumerable<TSource>, bool> predicate,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> @else)
        {
            return predicate(source) ? then(source) : @else(source);
        }

        public static IOrderedEnumerable<TResult> If<TSource, TResult>(
                this IEnumerable<TSource> source,
                Func<IEnumerable<TSource>, bool> predicate,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return predicate(source) ? then(source) : @else(source);
        }

        public static IOrderedEnumerable<TResult> If<TSource, TResult>(
                this IOrderedEnumerable<TSource> source,
                Func<IOrderedEnumerable<TSource>, bool> predicate,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return predicate(source) ? then(source) : @else(source);
        }



        public static TResult If<TSource, TResult>(
                this IEnumerable<TSource> source,
                Func<IEnumerable<TSource>, bool> predicate,
                Func<IEnumerable<TSource>, TResult> then,
                Func<IEnumerable<TSource>, TResult> @else)
        {
            return predicate(source) ? then(source) : @else(source);
        }

        public static TResult If<TSource, TResult>(
                this IOrderedEnumerable<TSource> source,
                Func<IOrderedEnumerable<TSource>, bool> predicate,
                Func<IOrderedEnumerable<TSource>, TResult> then,
                Func<IOrderedEnumerable<TSource>, TResult> @else)
        {
            return predicate(source) ? then(source) : @else(source);
        }



        public static IQueryable<TResult> If<TSource, TResult>(
                this IQueryable<TSource> source,
                bool cond,
                Func<IQueryable<TSource>, IQueryable<TResult>> then,
                Func<IQueryable<TSource>, IQueryable<TResult>> @else)
        {
            return cond ? then(source) : @else(source);
        }

        public static IOrderedQueryable<TResult> If<TSource, TResult>(
                this IQueryable<TSource> source,
                bool cond,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return cond ? then(source) : @else(source);
        }

        public static IOrderedQueryable<TResult> If<TSource, TResult>(
                this IOrderedQueryable<TSource> source,
                bool cond,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return cond ? then(source) : @else(source);
        }



        public static TResult If<TSource, TResult>(
                this IQueryable<TSource> source,
                bool cond,
                Func<IQueryable<TSource>, TResult> then,
                Func<IQueryable<TSource>, TResult> @else)
        {
            return cond ? then(source) : @else(source);
        }

        public static TResult If<TSource, TResult>(
                this IOrderedQueryable<TSource> source,
                bool cond,
                Func<IOrderedQueryable<TSource>, TResult> then,
                Func<IOrderedQueryable<TSource>, TResult> @else)
        {
            return cond ? then(source) : @else(source);
        }
    }
}
