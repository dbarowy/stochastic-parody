module Helper

let generateRandomLine (random: System.Random)  = 
    let candidate = "qwertyuiopasdfghjklzxcvbnm      "
    // let rnd = System.Random()
    let l = candidate.Length
    let inds = [1..100]

    let line = inds |> List.fold (fun acc x -> string candidate[random.Next(l)] + acc) "\n"
    
    line

let generateRandomLineWithSymbols (random: System.Random)  = 
    let candidate = "qwertyuiopasdfghjklzxcvbnm$$!![]={}      "
    // let rnd = System.Random()
    let l = candidate.Length
    let inds = [1..100]

    let line = inds |> List.fold (fun acc x -> string candidate[random.Next(l)] + acc) "\n"
    
    line