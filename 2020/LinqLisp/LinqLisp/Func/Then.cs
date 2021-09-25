using System;

namespace LinqLisp
{
    public static partial class Func
    {
        // Makes it possible to execute non-extension methods as if they were extension methods.
        // Threading macro equivalent.

        public static TOut Then<TIn, TOut>(this TIn input,
                Func<TIn, TOut> f1)
        {
            if (f1 == null) return default(TOut);
            return f1(input);
        }

        public static TOut Then<TIn, T1, TOut>(this TIn input,
                Func<TIn, T1> f1,
                Func<T1, TOut> f2)
        {
            if (f1 == null) return default(TOut);
            return f1(input).Then(f2);
        }

        public static TOut Then<TIn, T1, T2, TOut>(this TIn input,
                Func<TIn, T1> f1,
                Func<T1, T2> f2,
                Func<T2, TOut> f3)
        {
            if (f1 == null) return default(TOut);
            return f1(input).Then(f2, f3);
        }

        public static TOut Then<TIn, T1, T2, T3, TOut>(this TIn input,
                Func<TIn, T1> f1,
                Func<T1, T2> f2,
                Func<T2, T3> f3,
                Func<T3, TOut> f4)
        {
            if (f1 == null) return default(TOut);
            return f1(input).Then(f2, f3, f4);
        }


        public static TOut Then<TIn, T1, T2, T3, T4, TOut>(this TIn input,
                Func<TIn, T1> f1,
                Func<T1, T2> f2,
                Func<T2, T3> f3,
                Func<T3, T4> f4,
                Func<T4, TOut> f5)
        {
            if (f1 == null) return default(TOut);
            return f1(input).Then(f2, f3, f4, f5);
        }

        public static TOut Then<TIn, T1, T2, T3, T4, T5, TOut>(this TIn input,
                Func<TIn, T1> f1,
                Func<T1, T2> f2,
                Func<T2, T3> f3,
                Func<T3, T4> f4,
                Func<T4, T5> f5,
                Func<T5, TOut> f6)
        {
            if (f1 == null) return default(TOut);
            return f1(input).Then(f2, f3, f4, f5, f6);
        }

        public static TOut Then<TIn, T1, T2, T3, T4, T5, T6, TOut>(this TIn input,
                Func<TIn, T1> f1,
                Func<T1, T2> f2,
                Func<T2, T3> f3,
                Func<T3, T4> f4,
                Func<T4, T5> f5,
                Func<T5, T6> f6,
                Func<T6, TOut> f7)
        {
            if (f1 == null) return default(TOut);
            return f1(input).Then(f2, f3, f4, f5, f6, f7);
        }

        public static TOut Then<TIn, T1, T2, T3, T4, T5, T6, T7, TOut>(this TIn input,
                Func<TIn, T1> f1,
                Func<T1, T2> f2,
                Func<T2, T3> f3,
                Func<T3, T4> f4,
                Func<T4, T5> f5,
                Func<T5, T6> f6,
                Func<T6, T7> f7,
                Func<T7, TOut> f8)
        {
            if (f1 == null) return default(TOut);
            return f1(input).Then(f2, f3, f4, f5, f6, f7, f8);
        }

        public static TOut Then<TIn, T1, T2, T3, T4, T5, T6, T7, T8, TOut>(this TIn input,
                Func<TIn, T1> f1,
                Func<T1, T2> f2,
                Func<T2, T3> f3,
                Func<T3, T4> f4,
                Func<T4, T5> f5,
                Func<T5, T6> f6,
                Func<T6, T7> f7,
                Func<T7, T8> f8,
                Func<T8, TOut> f9)
        {
            if (f1 == null) return default(TOut);
            return f1(input).Then(f2, f3, f4, f5, f6, f7, f8, f9);
        }
    }
}
