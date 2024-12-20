module Vila

open System.IO

type Token =
    { Line: int; Col: int; Text: string }

/// <summary>Construct a lazy sequence of characters in a given file path.</summary>
let private readChar (fpath: string) = seq {
    use sr = new StreamReader(fpath)
    while not sr.EndOfStream do yield sr.Read() |> char
}

let private tokeniseFile (fpath) = seq {
    let line = 1
    for c in readChar(fpath) do
        //if c == '\n' then line <- line + 1
        yield { Line = line; Col = 0; Text = c.ToString() }
}

for c in readChar "Examples/assembly.cil" do
    printf "Char: %c\n" c

// TODO: proper reader that operates characterwise and manually constructs token strings rather than splitting.
// This would be faster and less memory intensive than the string splitting alternative.
// Enable reading in parallel?
// Need to record file, line and column positions to insert debug data into the generated CIL.

// Save built assembly to disk if required:
// https://learn.microsoft.com/en-us/dotnet/api/system.reflection.emit.assemblybuilder.save?view=netframework-4.8.1

// readLines "Examples/hello.cil"

// filepath -> lines -> tokens -> ast -> ...
//     repl -> lines -> tokens -> ast -> ...


// readFile "Examples/assembly.cil"
