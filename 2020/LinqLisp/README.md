# LinqLisp

Extension methods for Linq to make it more Lisp-like.

_2020-07-09 – 2021-04-24_


## Example

LinqLisp can do _a lot_ more than just this.  The following is just a crude
example of the `.When()` extension method.

```cs
// Before
public IEumerable<string> WithoutLinqLisp(IEnumerable<int> foo, bool filterOutNegative = false, Func<int, bool> optionalFilter = null, bool sort = true)
{
    var nums = foo;

    if (filterOutNegative)
    {
        nums = nums.Where(y => y > 0);
    }

    if (optionalFilter != null)
    {
        nums = nums.Where(optionalFilter);
    }

    if (sort)
    {
        nums = nums.OrderBy(y => y);
    }

    return nums.Select(x => x.ToString())
               .ToList();
}


// After
public IEnumerable<string> WithLinqLisp(IEnumerable<int> foo, bool filterOutNegative = false, Func<int, bool> optionalFilter = null, bool sort = true)
{
    return foo.When(filterOutNegative, x => x.Where(y => y > 0))
              .When(optionalFilter != null, x => x.Where(optionalFilter))
              .When(sort, x => x.OrderBy(y => y))
              .Select(x => x.ToString())
              .ToList();
}
```


## Legal

_Public domain.  No rights reserved._
