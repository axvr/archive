static void Main(string[] args)
{
    for (int i = 2; i < 1000; i++)
    {
        for (int j = 2; j < i; j++)
        {
             if (i % j == 0)
                 goto outer;
        }
        Console.WriteLine(i);
        outer:;
    }
}
