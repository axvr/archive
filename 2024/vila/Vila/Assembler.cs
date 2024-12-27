using System.Text;

namespace Vila.Assembler;

internal record Token(TokenType Type, string? Text, TokenLocation Loc);
internal record TokenLocation(Uri Source, uint Line, uint Col);

internal enum TokenType
{
    BeginScope, EndScope,
    BeginParams, EndParams, Comma,
    String, Integer, Float, Boolean, Null,
    Comment, BeginComment, EndComment,
    Declaration, Label, Symbol
}

// TODO: support peeking multiple chars ahead?
internal class CharReader(StreamReader sr, Uri source)
{
    public uint Line { get; private set; } = 1;
    public uint Col { get; private set; } = 0;
    public bool EndOfStream => sr.EndOfStream;
    public Uri Source => source;

    // TODO: cast to char.
    public int Read()
    {
        var c = sr.Read();
        if (c == -1) return c;
        var ch = (char)c;
        if (ch == '\n') { ++Line; Col = 0; }
        else ++Col;
        return c;
    }

    // TODO: read multiple chars into internal buffer and commit once done?
    //  ^ auto-capture the initial position.

    public int Peek() => sr.Peek();
}

internal static class Reader
{
    // TODO: the reader shouldn't open the stream?
    internal static StreamReader UriToStreamReader(Uri u) => u.Scheme switch
    {
        "file" => new StreamReader(u.AbsolutePath),
        _ => throw new NotImplementedException()
    };

    static Token ReadString(CharReader sr)
    {
        StringBuilder str = new();

        // Consume first delimiter.
        sr.Read();

        // Save initial line and col.
        uint line = sr.Line, col = sr.Col;

        while (!sr.EndOfStream)
        {
            var c = (char)sr.Read();
            if (c == '"') break;
            str.Append(c);
        }

        var token = new Token(
            Type: TokenType.String,
            Text: str.ToString(),
            Loc: new TokenLocation(sr.Source, line, col));

        return token;
    }

    static Token ReadSymbol(CharReader sr)
    {
        StringBuilder str = new();

        // Consume and store first char.
        var c = (char)sr.Read();
        str.Append(c);

        // Save initial line and col.
        uint line = sr.Line, col = sr.Col;

        while (!(sr.EndOfStream || char.IsWhiteSpace((char)sr.Peek())))
        {
            c = (char)sr.Read();
            str.Append(c);
        }

        var token = new Token(
            Type: TokenType.String,
            Text: str.ToString(),
            Loc: new TokenLocation(sr.Source, line, col));

        return token;
    }

    static Token ReadScopeDelim(CharReader sr)
    {
        var c = (char)sr.Read();
        return new Token(
            Type: c == '{' ? TokenType.BeginScope : TokenType.EndScope,
            Text: c.ToString(),
            Loc: new TokenLocation(sr.Source, sr.Line, sr.Col));
    }

    static Token ReadComma(CharReader sr)
    {
        sr.Read();  // Consume char.
        return new Token(
            Type: TokenType.Comma,
            Text: ",",
            Loc: new TokenLocation(sr.Source, sr.Line, sr.Col));
    }

    internal static IEnumerable<Token> ReadTokens(StreamReader sr, Uri source)
    {
        CharReader sr2 = new(sr, source);

        while (!sr2.EndOfStream)
        {
            var peekCh = (char)sr2.Peek();
            if (char.IsWhiteSpace(peekCh)) { sr2.Read(); continue; }

            // TODO: need something continuation-like, i.e. try this, if fail, try next (+ pass collected state)?

            // TODO: tokenise comments.
            yield return peekCh switch
            {
                '{' or '}' => ReadScopeDelim(sr2),
                ',' => ReadComma(sr2),
                '"' => ReadString(sr2),
                _ => ReadSymbol(sr2)
            };
        }
    }
}
