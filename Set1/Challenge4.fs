module Set1.Challenge4

(*

--Detect single-character XOR--

One of the 60-character strings in this file has been encrypted by single-character XOR.
Find it.

(Your code from #3 should help.)

*)

open Utility
open System

// hex-encoded strings, 1 per line
let data_path = @"..\..\challenge4_data.txt"
let run =
    IO.File.ReadAllLines (data_path)
    |> Array.map (fun enc -> 
        let key, dec = Util.findKeyFromHex_charXOR enc
        enc, key, dec)
    |> Array.maxBy (fun (_,_,s) -> Util.score s)
