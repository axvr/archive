using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;
// using LinqLisp;
// using static LinqLisp;
using static LinqLisp.Loop;

namespace LinqLisp.Tests
{
    public class UnitTest1
    {
        [Fact]
        public void Test1()
        {
            DoTimes(100, x => Console.WriteLine(x));

            // TODO: decide which namespace I prefer.

            // DoTimes(100, x => Console.WriteLine(x));
            // Loop.DoTimes(100, x => Console.WriteLine(x));
            // LinqLisp.Loop.DoTimes(100, x => Console.WriteLine(x));

            // DoTimes(100, x => Console.WriteLine(x));
            // LinqLisp.DoTimes(100, x => Console.WriteLine(x));
        }


        [Fact]
        public void Test2()
        {
            var strs = new List<string>();
            strs.When(true, then: x => x.Select(y => y.Trim()));
        }


        [Fact]
        public void Test3()
        {
            var strs = new List<string> { "1", "2", "3", "4", "5", "6" };

            Func<string, string> add1 = (x) => x + "1";
            "foo".Then(add1, add1, add1, add1);

            var foo = strs
                .Where(x => int.TryParse(x, out int _))
                .Then(x => string.Join(", ", x))
                .Then(AddHello);

            // var bar = strs
            //     .Where(x => int.TryParse(x, out int _))
            //     .Then(x => string.Join(", ", x));

            // "foo".Then(add1, add1, add1, add1);

            // strs.AsQueryable().Select();

            var a = add1.Then(add1).Then(add1);
            // var b = add1.Then(add1, add1);
            Console.WriteLine(a("Hello"));

            var add2 = add1.Then(add1);

            var bar = add1.Then(Uppercase).Then(AddHello);
            var biz = Func<string, string>.Combine(add1, add1);

            Func.Wrap<int, int>(Inc).Then(Inc);
            Func.Wrap<int, int>(x => x + 2).Then(Inc);

            // Func.Then(Uppercase, AddHello);
        }


        private string Uppercase(string input)
        {
            return input.ToUpper();
        }

        private string AddHello(string input)
        {
            return input + "hello";
        }

        private int Inc(int i)
        {
            return i + 1;
        }
    }
}
