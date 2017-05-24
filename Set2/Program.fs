module Set2.Main

open System
open Utility


[<EntryPoint>]
let main argv = 
    let padded = Challenge9.run
    assert(padded = "YELLOW SUBMARINE" + new string('\u0004',4))
    printfn "Challenge 9:\n  %s (length = %i)\n" padded padded.Length


    let decrypted, reencoded = Challenge10.run
    assert (decrypted.StartsWith "I'm back and I'm ringin' the bell ")
    assert (reencoded.StartsWith "CRIwqt4+szDbqkNY+I0qbNXPg1XLaCM5etQ5Bt9DRFV/xIN2k8Go7jtArLIy")
    printfn "Challenge 10:\n  decrypted: %s...\n  encrypted: %s...\n"
        <| decrypted.Substring(0, 33) <| reencoded.Substring(0, 33)



    Console.Read ()
