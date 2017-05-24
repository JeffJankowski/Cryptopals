module Set2.Main

open System
open Utility


[<EntryPoint>]
let main argv = 
    let padded = Challenge9.run
    assert(padded = "YELLOW SUBMARINE" + new string('\u0004',4))
    printfn "Challenge 9:\n  %s (length = %i)\n" padded padded.Length


    Console.Read ()
