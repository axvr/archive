using System;

namespace LinqLisp
{
    public static partial class Loop
    {
        // TODO: Add XML comments.

        // Example:
        // LinqLisp.Loop.DoTimes(100, x => Console.WriteLine(x));

        public static void DoTimes(long times, Action action) =>
            DoTimes(times, n => { action(); return true; });

        public static void DoTimes(ulong times, Action action) =>
            DoTimes(times, n => { action(); return true; });

        public static void DoTimes(int times, Action action) =>
            DoTimes(times, n => { action(); return true; });

        public static void DoTimes(uint times, Action action) =>
            DoTimes(times, n => { action(); return true; });

        public static void DoTimes(short times, Action action) =>
            DoTimes(times, n => { action(); return true; });

        public static void DoTimes(ushort times, Action action) =>
            DoTimes(times, n => { action(); return true; });

        public static void DoTimes(sbyte times, Action action) =>
            DoTimes(times, n => { action(); return true; });

        public static void DoTimes(byte times, Action action) =>
            DoTimes(times, n => { action(); return true; });



        public static void DoTimes(long times, Action<long> action) =>
            DoTimes(times, n => { action(n); return true; });

        public static void DoTimes(ulong times, Action<ulong> action) =>
            DoTimes(times, n => { action(n); return true; });

        public static void DoTimes(int times, Action<int> action) =>
            DoTimes(times, n => { action(n); return true; });

        public static void DoTimes(uint times, Action<uint> action) =>
            DoTimes(times, n => { action(n); return true; });

        public static void DoTimes(short times, Action<short> action) =>
            DoTimes(times, n => { action(n); return true; });

        public static void DoTimes(ushort times, Action<ushort> action) =>
            DoTimes(times, n => { action(n); return true; });

        public static void DoTimes(sbyte times, Action<sbyte> action) =>
            DoTimes(times, n => { action(n); return true; });

        public static void DoTimes(byte times, Action<byte> action) =>
            DoTimes(times, n => { action(n); return true; });



        public static void DoTimes(long times, Func<bool> func) =>
            DoTimes(times, n => func());

        public static void DoTimes(ulong times, Func<bool> func) =>
            DoTimes(times, n => func());

        public static void DoTimes(int times, Func<bool> func) =>
            DoTimes(times, n => func());

        public static void DoTimes(uint times, Func<bool> func) =>
            DoTimes(times, n => func());

        public static void DoTimes(short times, Func<bool> func) =>
            DoTimes(times, n => func());

        public static void DoTimes(ushort times, Func<bool> func) =>
            DoTimes(times, n => func());

        public static void DoTimes(sbyte times, Func<bool> func) =>
            DoTimes(times, n => func());

        public static void DoTimes(byte times, Func<bool> func) =>
            DoTimes(times, n => func());



        public static void DoTimes(long times, Func<long, bool> func)
        {
            if (times < 1) return;
            for (long i = 0; i < times; i++)
                if (!func(i)) break;
        }

        public static void DoTimes(ulong times, Func<ulong, bool> func)
        {
            if (times < 1) return;
            for (ulong i = 0; i < times; i++)
                if (!func(i)) break;
        }

        public static void DoTimes(int times, Func<int, bool> func)
        {
            if (times < 1) return;
            for (int i = 0; i < times; i++)
                if (!func(i)) break;
        }

        public static void DoTimes(uint times, Func<uint, bool> func)
        {
            if (times < 1) return;
            for (uint i = 0; i < times; i++)
                if (!func(i)) break;
        }

        public static void DoTimes(short times, Func<short, bool> func)
        {
            if (times < 1) return;
            for (short i = 0; i < times; i++)
                if (!func(i)) break;
        }

        public static void DoTimes(ushort times, Func<ushort, bool> func)
        {
            if (times < 1) return;
            for (ushort i = 0; i < times; i++)
                if (!func(i)) break;
        }

        public static void DoTimes(byte times, Func<byte, bool> func)
        {
            if (times < 1) return;
            for (byte i = 0; i < times; i++)
                if (!func(i)) break;
        }

        public static void DoTimes(sbyte times, Func<sbyte, bool> func)
        {
            if (times < 1) return;
            for (sbyte i = 0; i < times; i++)
                if (!func(i)) break;
        }
    }
}
