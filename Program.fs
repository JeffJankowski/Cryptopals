open System
open System.Security.Cryptography

let hexToBytes (hexStr:string) = 
    hexStr
    |> List.ofSeq
    |> List.chunkBySize 2
    |> List.map (fun l ->
        let substr = l |> Array.ofList |> String
        Convert.ToByte (substr, 16) )
    |> List.toArray

let hexToBase64 (hexStr:string) =
    Convert.ToBase64String (hexToBytes hexStr)

let bytesToHex (bytes:byte[]) = 
    bytes
    |> Array.map (fun x -> String.Format ("{0:x2}", x))
    |> String.concat String.Empty

let xorByteArrays (arr1:byte[]) (arr2:byte[]) = 
    Array.map2 (fun b1 b2 -> b1 ^^^ b2) arr1 arr2

let xorBytes (arr:byte[]) (b:byte) = 
    Array.map (fun b1 -> b1 ^^^ b) arr

let score (s:string) = 
    s |> Seq.filter (fun c -> "etaoi ".Contains(c.ToString().ToLower())) |> Seq.length

let decrypt (encBytes:byte[]) = 
    [32..126]
    |> List.map (fun i -> 
        ((char i), xorBytes encBytes (byte i) |> Text.Encoding.ASCII.GetString))
    |> List.maxBy (fun (_, s) -> score s)

let decryptHex (encStr:string) =
    decrypt (hexToBytes encStr)

let strToBytes (str:string) = 
    str |> Seq.map byte |> Seq.toArray

let encrypt (str:string) (key:string) = 
    let repeated = 
        String.replicate (str.Length / key.Length + 1) key
        |> strToBytes
        |> Array.take str.Length
    xorByteArrays (strToBytes str) repeated |> bytesToHex

let weight (b:byte) = 
    [0..7]
    |> List.sumBy (fun i -> int ((b >>> i) &&& 1uy) )
        
let hamming (b1:byte[]) (b2:byte[]) = 
    xorByteArrays b1 b2 |> Array.sumBy weight

let decryptWithKey (enc:byte[]) (key:string) = 
    let repeated = 
        String.replicate (enc.Length / key.Length + 1) key
        |> strToBytes
        |> Array.take enc.Length
    xorByteArrays enc repeated |> Text.Encoding.ASCII.GetString


[<EntryPoint>]
let main argv = 
    // challenge 1
    let hex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    hexToBase64 hex |> printfn "Challenge 1:\n  %s\n"

    // challenge 2
    let a = "1c0111001f010100061a024b53535009181c"
    let b = "686974207468652062756c6c277320657965"
    xorByteArrays (hexToBytes a) (hexToBytes b)
    |> bytesToHex
    |> printfn "Challenge 2:\n  %s\n"

    // challenge 3
    let encrypted = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    let ch, dec = decryptHex encrypted
    printfn "Challenge 3:\n  %c: %s\n" ch dec

    // challenge 4
    let enc2, ch2, dec2 = 
        IO.File.ReadAllLines ("..\\..\\challenge4_data.txt")
        |> Array.map (fun enc -> 
            let c,d = decryptHex enc
            enc, c, d)
        |> Array.maxBy (fun (_,_,s) -> score s)
    printfn "Challenge 4:\n  %c: %s (%s)\n" ch2 dec2 enc2

    // challenge 5
    let key = "ICE"
    let text = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    printfn "Challenge 5:\n  %s\n" (encrypt text key)

    // challenge 6
    let dataBytes = 
        IO.File.ReadAllLines ("..\\..\\challenge6_data.txt")
        |> String.Concat
        |> Convert.FromBase64String
    let keySz = 
        [2..40]
        |> List.minBy (fun i -> 
            let n = 2
            dataBytes
            |> Seq.chunkBySize i
            |> Seq.chunkBySize 2
            |> Seq.filter (fun chunk -> 
                chunk.Length > 1 && chunk.[0].Length = i && chunk.[1].Length = i)
            |> Seq.map (fun chunk -> (hamming chunk.[0] chunk.[1] |> float) / (float i))
            |> Seq.average)
    let chunks = dataBytes |> Array.chunkBySize keySz
    let key =
        [0..keySz-1]
        |> List.map (fun i -> 
            chunks 
            |> Array.choose (fun chunk -> if chunk.Length > i then Some(chunk.[i]) else None)
            |> decrypt
            |> fst)
        |> String.Concat
    printfn "Challenge 6:\n  %s\n" key
//    printfn "%s" (decryptWithKey dataBytes key)

    // challenge 7
    let key = "YELLOW SUBMARINE"


    Console.Read ()
