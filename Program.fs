module Set1.Main

open System
open Utility


[<EntryPoint>]
let main argv = 
    let res = Challenge1.run
    assert (res = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
    printfn "Challenge 1:\n  %s\n" res
    

    let res = Challenge2.run
    assert (res = "746865206b696420646f6e277420706c6179")
    printfn "Challenge 2:\n  %s\n" res


    let key, dec = Challenge3.run
    assert (key = 'X')
    assert (dec = "Cooking MC's like a pound of bacon")
    printfn "Challenge 3:\n  %c: %s\n" key dec
    

    let enc, key, dec = Challenge4.run
    assert (key = '5')
    assert (dec = "Now that the party is jumping\n")
    assert (enc = "7b5a4215415d544115415d5015455447414c155c46155f4058455c5b523f")
    printfn "Challenge 4:\n  %c: %s (%s)\n" key dec enc


    let res = Challenge5.run
    assert (res = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a2622632427276527" + 
                  "2a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")
    printfn "Challenge 5:\n  %s\n" res


    let key, dec = Challenge6.run
    assert (key = "Terminator X: Bring the noise")
    assert (dec.Split '\n' |> Array.head = "I'm back and I'm ringin' the bell ")
    printfn "Challenge 6:\n  %s\n" key


    let res = Challenge7.run
    let first = dec.Split '\n' |> Array.head
    assert (first = "I'm back and I'm ringin' the bell ")
    printfn "Challenge 7:\n  %s\n  ...\n" first


    Console.Read ()
