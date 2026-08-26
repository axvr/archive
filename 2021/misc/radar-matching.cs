// Assumes scores are ints between 0 and 10.
private static readonly int[] squareLookup = new int[11] { 0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100 };

private static int Distance(List<int> user, List<int> brand, bool orBetter = false, bool discountZero = false)
{
    return user.Zip(brand, (u, b) =>
    {
        if (orBetter && b > u) return 0;
        if (discountZero && (b == 0 || u == 0)) return 0;
        return squareLookup[Math.Abs(u - b)];
    }).Sum();
}

// .OrderBy(x => Distance(x.Scores, brandScores, orBetter: true, discountZero: true))
// .ThenByDescending(x => brandScores.Sum())
