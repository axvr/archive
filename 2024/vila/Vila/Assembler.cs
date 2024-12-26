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

internal class LocationAwareStreamReader : StreamReader
{
    public uint Line { get; private set; } = 1;
    public uint Col { get; private set; } = 0;

    // FIXME: Terrible location tracking.  Doesn't work with Async, Block or Linewise reading.
    public override int Read()
    {
        var c = base.Read();
        if ((char)c == '\n') { ++Line; Col = 0; }
        else if (c >= 0) ++Col;
        return c;
    }

    // Remove other "Read" implementations that won't work with location tracking.
    public override int Read(Span<char> buffer) => throw new NotImplementedException();
    public override int Read(char[] buffer, int index, int count) => throw new NotImplementedException();
    public override ValueTask<int> ReadAsync(Memory<char> buffer, CancellationToken cancellationToken = default) => throw new NotImplementedException();
    public override Task<int> ReadAsync(char[] buffer, int index, int count) => throw new NotImplementedException();
    public override int ReadBlock(Span<char> buffer) => throw new NotImplementedException();
    public override int ReadBlock(char[] buffer, int index, int count) => throw new NotImplementedException();
    public override ValueTask<int> ReadBlockAsync(Memory<char> buffer, CancellationToken cancellationToken = default) => throw new NotImplementedException();
    public override Task<int> ReadBlockAsync(char[] buffer, int index, int count) => throw new NotImplementedException();
    public override string? ReadLine() => throw new NotImplementedException();
    public override Task<string?> ReadLineAsync() => throw new NotImplementedException();
    public override ValueTask<string?> ReadLineAsync(CancellationToken cancellationToken) => throw new NotImplementedException();
    public override string ReadToEnd() => throw new NotImplementedException();
    public override Task<string> ReadToEndAsync() => throw new NotImplementedException();
    public override Task<string> ReadToEndAsync(CancellationToken cancellationToken) => throw new NotImplementedException();

    // Default constructors.
    public LocationAwareStreamReader(Stream stream) : base(stream) { }
    public LocationAwareStreamReader(string path) : base(path) { }
    public LocationAwareStreamReader(Stream stream, bool detectEncodingFromByteOrderMarks) : base(stream, detectEncodingFromByteOrderMarks) { }
    public LocationAwareStreamReader(Stream stream, Encoding encoding) : base(stream, encoding) { }
    public LocationAwareStreamReader(string path, bool detectEncodingFromByteOrderMarks) : base(path, detectEncodingFromByteOrderMarks) { }
    public LocationAwareStreamReader(string path, FileStreamOptions options) : base(path, options) { }
    public LocationAwareStreamReader(string path, Encoding encoding) : base(path, encoding) { }
    public LocationAwareStreamReader(Stream stream, Encoding encoding, bool detectEncodingFromByteOrderMarks) : base(stream, encoding, detectEncodingFromByteOrderMarks) { }
    public LocationAwareStreamReader(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks) : base(path, encoding, detectEncodingFromByteOrderMarks) { }
    public LocationAwareStreamReader(Stream stream, Encoding encoding, bool detectEncodingFromByteOrderMarks, int bufferSize) : base(stream, encoding, detectEncodingFromByteOrderMarks, bufferSize) { }
    public LocationAwareStreamReader(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks, int bufferSize) : base(path, encoding, detectEncodingFromByteOrderMarks, bufferSize) { }
    public LocationAwareStreamReader(string path, Encoding encoding, bool detectEncodingFromByteOrderMarks, FileStreamOptions options) : base(path, encoding, detectEncodingFromByteOrderMarks, options) { }
    public LocationAwareStreamReader(Stream stream, Encoding? encoding = null, bool detectEncodingFromByteOrderMarks = true, int bufferSize = -1, bool leaveOpen = false) : base(stream, encoding, detectEncodingFromByteOrderMarks, bufferSize, leaveOpen) { }
}

internal static class Reader
{
    // TODO: the reader shouldn't open the stream?
    internal static LocationAwareStreamReader UriToStreamReader(Uri u) => u.Scheme switch
    {
        "file" => new LocationAwareStreamReader(u.AbsolutePath),
        _ => throw new NotImplementedException()
    };

    static Token ReadString(LocationAwareStreamReader sr, Uri src)
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
            Loc: new TokenLocation(src, line, col));

        return token;
    }

    static Token ReadSymbol(LocationAwareStreamReader sr, Uri src)
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
            Loc: new TokenLocation(src, line, col));

        return token;
    }

    static Token ReadScopeDelim(LocationAwareStreamReader sr, Uri src)
    {
        var c = (char)sr.Read();
        return new Token(
            Type: c == '{' ? TokenType.BeginScope : TokenType.EndScope,
            Text: c.ToString(),
            Loc: new TokenLocation(src, sr.Line, sr.Col));
    }

    static Token ReadComma(LocationAwareStreamReader sr, Uri src)
    {
        var c = (char)sr.Read();
        return new Token(
            Type: TokenType.Comma,
            Text: ",",
            Loc: new TokenLocation(src, sr.Line, sr.Col));
    }

    internal static IEnumerable<Token> ReadTokens(LocationAwareStreamReader sr, Uri source)
    {
        while (!sr.EndOfStream)
        {
            var peekCh = (char)sr.Peek();
            if (char.IsWhiteSpace(peekCh)) { sr.Read(); continue; }

            // TODO: need something continuation-like, i.e. try this, if fail, try next (+ pass collected state)?

            // TODO: tokenise comments.
            yield return peekCh switch
            {
                '{' or '}' => ReadScopeDelim(sr, source),
                ',' => ReadComma(sr, source),
                '"' => ReadString(sr, source),
                _ => ReadSymbol(sr, source)
            };
        }
    }
}
