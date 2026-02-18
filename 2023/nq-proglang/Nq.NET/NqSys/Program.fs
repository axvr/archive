open Experiment

let dll1 = "/Users/axvr/Projects/codedump/2024/DotNetTest/DotNetTest1/bin/Debug/net8.0/DotNetTest1.dll"
let dll2 = "/Users/axvr/Projects/codedump/2024/DotNetTest/DotNetTest2/bin/Debug/net8.0/DotNetTest2.dll"

let rec testSystemRepeatLoad loops =
    printf "Count: %i\n" loops
    startSystem dll1
    startSystem dll2
    if loops > 0 then testSystemRepeatLoad (loops - 1)

[<EntryPoint>]
let main args =
    testSystemRepeatLoad 1
    0
