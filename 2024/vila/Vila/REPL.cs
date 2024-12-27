using Vila.Assembler;

namespace Vila.REPL;

internal class Program
{
    public static void Main()
    {
        Uri f = new("file:///Users/axvr/Projects/Experiments/vila/Examples/hello.cil");
        Console.WriteLine(f.AbsolutePath);
        using var sr = Reader.UriToStreamReader(f);
        foreach (var token in Reader.ReadTokens(sr, f)) Console.WriteLine(token);
    }
}
