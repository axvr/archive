open System.Text

module Vila.Assembler

type internal Token = { Type: TokenType; Text: Option<string>; Loc: TokenLocation }
type internal TokenLocation = { Source: Uri; Line: uint; Col: uint }
type internal TokenType =
    | BeginScope = 0 | EndScope = 1
    | BeginParams = 2 | EndParams = 3 | Comma = 4
    | String = 5 | Integer = 6 | Float = 7 | Boolean = 8 | Null = 9
    | Comment = 10 | BeginComment = 11 | EndComment = 12
    | Declaration = 13 | Label = 14 | Symbol = 15

let internal uriToStreamReader u: Uri =
    match u.Scheme with
    | "file" -> new StreamReader(u.AbsolutePath)
    | _ -> raise (NotImplementedException())

let internal readComma sr: CharReader =
    sr.Read()  // Consume char.
    { Type = TokenType.Comma
      Text = ","
      Loc = { Source = sr.Source; Line = sr.Line; Col = sr.Col } }

let internal readScopeDelim sr: CharReader =
    let c = sr.Read |> char
    { Type = if c == '{' then TokenType.BeginScope else TokenType.EndScope
      Text = c.ToString()
      Loc = { Source = sr.Source; Line = sr.Line; Col = sr.Col } }

// type internal CharReader(sr: StreamReader, source: Uri) =
//     let mutable Line

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
