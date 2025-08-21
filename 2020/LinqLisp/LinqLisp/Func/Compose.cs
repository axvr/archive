using System;

namespace LinqLisp
{
    // Compose functions.

    public static partial class Func
    {
        public static Func<TIn, TOut> Then<TIn, T1, TOut>(
                this Func<TIn, T1> f1,
                Func<T1, TOut> f2)
        {
            return (x) => f2(f1(x));
        }

        /// <summary>
        /// Allows you to wrap a lambda function or method to use it at the start of a <see cref="Then{TIn,T1,TOut}"/> chain.
        /// </summary>
        public static Func<TIn, TOut> Wrap<TIn, TOut>(Func<TIn, TOut> f) => f;
    }
}
