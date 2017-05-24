module Set1.Challenge8

(*

--Detect AES in ECB mode--

In this file are a bunch of hex-encoded ciphertexts.
One of them has been encrypted with ECB.

Detect it.

Remember that the problem with ECB is that it is stateless and deterministic; the same 16 byte 
plaintext block will always produce the same 16 byte ciphertext.

*)

open Utility
open System

let dups (line:string) = 
    line
    |> Seq.chunkBySize 16
    |> Seq.groupBy (fun chunk -> new string(chunk))
    |> Seq.map (fun (_, grp) -> grp |> Seq.length)
    |> Seq.max

// hex-encoded lines
let data_path = @"..\..\challenge8_data.txt"
let run = 
    IO.File.ReadAllLines (data_path)
    |> Array.map (fun line -> (dups line), line)
    |> Array.maxBy fst