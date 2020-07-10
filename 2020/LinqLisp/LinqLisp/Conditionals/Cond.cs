using System;
using System.Collections.Generic;
using System.Linq;

namespace LinqLisp
{
    public static partial class Conditionals
    {
        public static IEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then1,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, @else);
        }

        public static IEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then1,
                bool cond2,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then2,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, @else));
        }

        public static IEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then1,
                bool cond2,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then2,
                bool cond3,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then3,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, @else));
        }

        public static IEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then1,
                bool cond2,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then2,
                bool cond3,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then3,
                bool cond4,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then4,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, @else));
        }

        public static IEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then1,
                bool cond2,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then2,
                bool cond3,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then3,
                bool cond4,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then4,
                bool cond5,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then5,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, @else));
        }

        public static IEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then1,
                bool cond2,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then2,
                bool cond3,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then3,
                bool cond4,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then4,
                bool cond5,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then5,
                bool cond6,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then6,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, @else));
        }

        public static IEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then1,
                bool cond2,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then2,
                bool cond3,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then3,
                bool cond4,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then4,
                bool cond5,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then5,
                bool cond6,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then6,
                bool cond7,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then7,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, @else));
        }

        public static IEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then1,
                bool cond2,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then2,
                bool cond3,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then3,
                bool cond4,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then4,
                bool cond5,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then5,
                bool cond6,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then6,
                bool cond7,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then7,
                bool cond8,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then8,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, cond8, then8, @else));
        }

        public static IEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then1,
                bool cond2,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then2,
                bool cond3,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then3,
                bool cond4,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then4,
                bool cond5,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then5,
                bool cond6,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then6,
                bool cond7,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then7,
                bool cond8,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then8,
                bool cond9,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> then9,
                Func<IEnumerable<TSource>, IEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, cond8, then8, cond9, then9, @else));
        }



        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, @else);
        }

        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                bool cond2,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then2,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, @else));
        }

        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                bool cond2,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then2,
                bool cond3,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then3,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, @else));
        }

        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                bool cond2,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then2,
                bool cond3,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then3,
                bool cond4,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then4,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, @else));
        }

        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                bool cond2,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then2,
                bool cond3,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then3,
                bool cond4,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then4,
                bool cond5,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then5,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, @else));
        }

        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                bool cond2,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then2,
                bool cond3,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then3,
                bool cond4,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then4,
                bool cond5,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then5,
                bool cond6,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then6,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, @else));
        }

        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                bool cond2,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then2,
                bool cond3,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then3,
                bool cond4,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then4,
                bool cond5,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then5,
                bool cond6,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then6,
                bool cond7,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then7,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, @else));
        }

        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                bool cond2,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then2,
                bool cond3,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then3,
                bool cond4,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then4,
                bool cond5,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then5,
                bool cond6,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then6,
                bool cond7,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then7,
                bool cond8,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then8,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, cond8, then8, @else));
        }

        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IEnumerable<TSource> source,
                bool cond1,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                bool cond2,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then2,
                bool cond3,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then3,
                bool cond4,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then4,
                bool cond5,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then5,
                bool cond6,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then6,
                bool cond7,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then7,
                bool cond8,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then8,
                bool cond9,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> then9,
                Func<IEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, cond8, then8, cond9, then9, @else));
        }



        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IOrderedEnumerable<TSource> source,
                bool cond1,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, @else);
        }

        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IOrderedEnumerable<TSource> source,
                bool cond1,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                bool cond2,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then2,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, @else));
        }

        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IOrderedEnumerable<TSource> source,
                bool cond1,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                bool cond2,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then2,
                bool cond3,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then3,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, @else));
        }

        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IOrderedEnumerable<TSource> source,
                bool cond1,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                bool cond2,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then2,
                bool cond3,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then3,
                bool cond4,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then4,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, @else));
        }

        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IOrderedEnumerable<TSource> source,
                bool cond1,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                bool cond2,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then2,
                bool cond3,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then3,
                bool cond4,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then4,
                bool cond5,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then5,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, @else));
        }

        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IOrderedEnumerable<TSource> source,
                bool cond1,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                bool cond2,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then2,
                bool cond3,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then3,
                bool cond4,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then4,
                bool cond5,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then5,
                bool cond6,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then6,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, @else));
        }

        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IOrderedEnumerable<TSource> source,
                bool cond1,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                bool cond2,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then2,
                bool cond3,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then3,
                bool cond4,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then4,
                bool cond5,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then5,
                bool cond6,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then6,
                bool cond7,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then7,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, @else));
        }

        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IOrderedEnumerable<TSource> source,
                bool cond1,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                bool cond2,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then2,
                bool cond3,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then3,
                bool cond4,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then4,
                bool cond5,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then5,
                bool cond6,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then6,
                bool cond7,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then7,
                bool cond8,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then8,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, cond8, then8, @else));
        }

        public static IOrderedEnumerable<TResult> Cond<TSource, TResult>(this IOrderedEnumerable<TSource> source,
                bool cond1,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then1,
                bool cond2,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then2,
                bool cond3,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then3,
                bool cond4,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then4,
                bool cond5,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then5,
                bool cond6,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then6,
                bool cond7,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then7,
                bool cond8,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then8,
                bool cond9,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> then9,
                Func<IOrderedEnumerable<TSource>, IOrderedEnumerable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, cond8, then8, cond9, then9, @else));
        }



        public static IQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IQueryable<TResult>> then1,
                Func<IQueryable<TSource>, IQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, @else);
        }

        public static IQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IQueryable<TResult>> then1,
                bool cond2,
                Func<IQueryable<TSource>, IQueryable<TResult>> then2,
                Func<IQueryable<TSource>, IQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, @else));
        }

        public static IQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IQueryable<TResult>> then1,
                bool cond2,
                Func<IQueryable<TSource>, IQueryable<TResult>> then2,
                bool cond3,
                Func<IQueryable<TSource>, IQueryable<TResult>> then3,
                Func<IQueryable<TSource>, IQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, @else));
        }

        public static IQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IQueryable<TResult>> then1,
                bool cond2,
                Func<IQueryable<TSource>, IQueryable<TResult>> then2,
                bool cond3,
                Func<IQueryable<TSource>, IQueryable<TResult>> then3,
                bool cond4,
                Func<IQueryable<TSource>, IQueryable<TResult>> then4,
                Func<IQueryable<TSource>, IQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, @else));
        }

        public static IQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IQueryable<TResult>> then1,
                bool cond2,
                Func<IQueryable<TSource>, IQueryable<TResult>> then2,
                bool cond3,
                Func<IQueryable<TSource>, IQueryable<TResult>> then3,
                bool cond4,
                Func<IQueryable<TSource>, IQueryable<TResult>> then4,
                bool cond5,
                Func<IQueryable<TSource>, IQueryable<TResult>> then5,
                Func<IQueryable<TSource>, IQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, @else));
        }

        public static IQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IQueryable<TResult>> then1,
                bool cond2,
                Func<IQueryable<TSource>, IQueryable<TResult>> then2,
                bool cond3,
                Func<IQueryable<TSource>, IQueryable<TResult>> then3,
                bool cond4,
                Func<IQueryable<TSource>, IQueryable<TResult>> then4,
                bool cond5,
                Func<IQueryable<TSource>, IQueryable<TResult>> then5,
                bool cond6,
                Func<IQueryable<TSource>, IQueryable<TResult>> then6,
                Func<IQueryable<TSource>, IQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, @else));
        }

        public static IQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IQueryable<TResult>> then1,
                bool cond2,
                Func<IQueryable<TSource>, IQueryable<TResult>> then2,
                bool cond3,
                Func<IQueryable<TSource>, IQueryable<TResult>> then3,
                bool cond4,
                Func<IQueryable<TSource>, IQueryable<TResult>> then4,
                bool cond5,
                Func<IQueryable<TSource>, IQueryable<TResult>> then5,
                bool cond6,
                Func<IQueryable<TSource>, IQueryable<TResult>> then6,
                bool cond7,
                Func<IQueryable<TSource>, IQueryable<TResult>> then7,
                Func<IQueryable<TSource>, IQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, @else));
        }

        public static IQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IQueryable<TResult>> then1,
                bool cond2,
                Func<IQueryable<TSource>, IQueryable<TResult>> then2,
                bool cond3,
                Func<IQueryable<TSource>, IQueryable<TResult>> then3,
                bool cond4,
                Func<IQueryable<TSource>, IQueryable<TResult>> then4,
                bool cond5,
                Func<IQueryable<TSource>, IQueryable<TResult>> then5,
                bool cond6,
                Func<IQueryable<TSource>, IQueryable<TResult>> then6,
                bool cond7,
                Func<IQueryable<TSource>, IQueryable<TResult>> then7,
                bool cond8,
                Func<IQueryable<TSource>, IQueryable<TResult>> then8,
                Func<IQueryable<TSource>, IQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, cond8, then8, @else));
        }

        public static IQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IQueryable<TResult>> then1,
                bool cond2,
                Func<IQueryable<TSource>, IQueryable<TResult>> then2,
                bool cond3,
                Func<IQueryable<TSource>, IQueryable<TResult>> then3,
                bool cond4,
                Func<IQueryable<TSource>, IQueryable<TResult>> then4,
                bool cond5,
                Func<IQueryable<TSource>, IQueryable<TResult>> then5,
                bool cond6,
                Func<IQueryable<TSource>, IQueryable<TResult>> then6,
                bool cond7,
                Func<IQueryable<TSource>, IQueryable<TResult>> then7,
                bool cond8,
                Func<IQueryable<TSource>, IQueryable<TResult>> then8,
                bool cond9,
                Func<IQueryable<TSource>, IQueryable<TResult>> then9,
                Func<IQueryable<TSource>, IQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, cond8, then8, cond9, then9, @else));
        }



        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, @else);
        }

        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                bool cond2,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then2,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, @else));
        }

        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                bool cond2,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then2,
                bool cond3,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then3,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, @else));
        }

        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                bool cond2,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then2,
                bool cond3,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then3,
                bool cond4,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then4,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, @else));
        }

        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                bool cond2,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then2,
                bool cond3,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then3,
                bool cond4,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then4,
                bool cond5,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then5,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, @else));
        }

        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                bool cond2,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then2,
                bool cond3,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then3,
                bool cond4,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then4,
                bool cond5,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then5,
                bool cond6,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then6,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, @else));
        }

        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                bool cond2,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then2,
                bool cond3,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then3,
                bool cond4,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then4,
                bool cond5,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then5,
                bool cond6,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then6,
                bool cond7,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then7,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, @else));
        }

        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                bool cond2,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then2,
                bool cond3,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then3,
                bool cond4,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then4,
                bool cond5,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then5,
                bool cond6,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then6,
                bool cond7,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then7,
                bool cond8,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then8,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, cond8, then8, @else));
        }

        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IQueryable<TSource> source,
                bool cond1,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                bool cond2,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then2,
                bool cond3,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then3,
                bool cond4,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then4,
                bool cond5,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then5,
                bool cond6,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then6,
                bool cond7,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then7,
                bool cond8,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then8,
                bool cond9,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> then9,
                Func<IQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, cond8, then8, cond9, then9, @else));
        }



        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IOrderedQueryable<TSource> source,
                bool cond1,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, @else);
        }

        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IOrderedQueryable<TSource> source,
                bool cond1,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                bool cond2,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then2,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, @else));
        }

        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IOrderedQueryable<TSource> source,
                bool cond1,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                bool cond2,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then2,
                bool cond3,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then3,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, @else));
        }

        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IOrderedQueryable<TSource> source,
                bool cond1,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                bool cond2,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then2,
                bool cond3,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then3,
                bool cond4,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then4,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, @else));
        }

        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IOrderedQueryable<TSource> source,
                bool cond1,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                bool cond2,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then2,
                bool cond3,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then3,
                bool cond4,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then4,
                bool cond5,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then5,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, @else));
        }

        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IOrderedQueryable<TSource> source,
                bool cond1,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                bool cond2,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then2,
                bool cond3,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then3,
                bool cond4,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then4,
                bool cond5,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then5,
                bool cond6,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then6,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, @else));
        }

        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IOrderedQueryable<TSource> source,
                bool cond1,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                bool cond2,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then2,
                bool cond3,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then3,
                bool cond4,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then4,
                bool cond5,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then5,
                bool cond6,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then6,
                bool cond7,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then7,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, @else));
        }

        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IOrderedQueryable<TSource> source,
                bool cond1,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                bool cond2,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then2,
                bool cond3,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then3,
                bool cond4,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then4,
                bool cond5,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then5,
                bool cond6,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then6,
                bool cond7,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then7,
                bool cond8,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then8,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, cond8, then8, @else));
        }

        public static IOrderedQueryable<TResult> Cond<TSource, TResult>(this IOrderedQueryable<TSource> source,
                bool cond1,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then1,
                bool cond2,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then2,
                bool cond3,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then3,
                bool cond4,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then4,
                bool cond5,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then5,
                bool cond6,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then6,
                bool cond7,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then7,
                bool cond8,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then8,
                bool cond9,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> then9,
                Func<IOrderedQueryable<TSource>, IOrderedQueryable<TResult>> @else)
        {
            return source.If(cond1, then1, x => x.Cond(cond2, then2, cond3, then3, cond4, then4, cond5, then5, cond6, then6, cond7, then7, cond8, then8, cond9, then9, @else));
        }
    }
}
