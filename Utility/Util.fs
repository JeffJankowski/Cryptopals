module Utility.Util

open System
open System.Security.Cryptography


let hexToByteArray (hexStr:string) = 
    hexStr
    |> List.ofSeq
    |> List.chunkBySize 2
    |> List.map (fun l ->
        let substr = l |> Array.ofList |> String
        Convert.ToByte (substr, 16) )
    |> List.toArray

let hexToBase64 (hexStr:string) =
    Convert.ToBase64String (hexToByteArray hexStr)

let byteArrayToHex (bytes:byte[]) = 
    bytes
    |> Array.map (fun x -> String.Format ("{0:x2}", x))
    |> String.concat String.Empty

let xorByteArrays (arr1:byte[]) (arr2:byte[]) = 
    Array.map2 (fun b1 b2 -> b1 ^^^ b2) arr1 arr2

let xorBytes (arr:byte[]) (b:byte) = 
    Array.map (fun b1 -> b1 ^^^ b) arr

let stringToByteArray (str:string) = 
    str |> Seq.map byte |> Seq.toArray

let byteArrayToString (data:byte[]) = 
    Text.Encoding.ASCII.GetString (data)


let encryptBytes_repeatedXOR (data:byte[]) (key:string) = 
    let repeated = 
        String.replicate (data.Length / key.Length + 1) key
        |> stringToByteArray
        |> Array.take data.Length
    xorByteArrays data repeated


let weight (b:byte) =
    [0..7]
    |> List.sumBy (fun i -> int ((b >>> i) &&& 1uy) )
        
let hamming (b1:byte[]) (b2:byte[]) = 
    xorByteArrays b1 b2 |> Array.sumBy weight



let score (s:string) = 
    s |> Seq.filter (fun c -> "etaoi ".Contains(c.ToString().ToLower())) |> Seq.length

let findKey_charXOR (encBytes:byte[]) = 
    [32..126]
    |> List.map (fun i -> 
        ((char i), xorBytes encBytes (byte i) |> byteArrayToString))
    |> List.maxBy (fun (_, s) -> score s)

let findKeyFromHex_charXOR (encStr:string) =
    findKey_charXOR (hexToByteArray encStr)


let decryptAES_ECB (data:byte[]) (key:byte[]) = 
    use crypto = new RijndaelManaged()
    crypto.KeySize <- key.Length * 8
    crypto.Key <- key
    crypto.Mode <- CipherMode.ECB;
    crypto.Padding <- PaddingMode.None
    let xform = crypto.CreateDecryptor()
    xform.TransformFinalBlock(data, 0, data.Length)


let padByteArray (data:byte[]) (n:int) = 
    Array.append data (Array.create (n - data.Length) 4uy)