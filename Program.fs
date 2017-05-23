open System


let hexToBase64 (hexStr:string) =
    hexStr
    |> List.ofSeq
    |> List.chunkBySize 2
    |> List.map (fun l ->
        let substr = l |> Array.ofList |> String
        Convert.ToByte (substr, 16) )
    |> List.toArray
    |> Convert.ToBase64String



[<EntryPoint>]
let main argv = 
    // challenge 1
    let hex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    hexToBase64 hex |> printfn "%s"

    // challenge 2


    Console.Read ()
