module Helper

// helper method to generate random string of words with random character
// essentially generates garbage input
let generateRandomLine (random: System.Random)  = 
    let candidate = "qwertyuiopasdfghjklzxcvbnm      "
    // let rnd = System.Random()
    let l = candidate.Length
    let inds = [1..100]

    let line = inds |> List.fold (fun acc x -> string candidate[random.Next(l)] + acc) "\n"
    
    line

// helper method to generate random string of words with random characters
// essentially generates garbage input
// also includes special characters
let generateRandomLineWithSymbols (random: System.Random)  = 
    let candidate = "qwertyuiopasdfghjklzxcvbnm$$!![]={}      "
    // let rnd = System.Random()
    let l = candidate.Length
    let inds = [1..100]

    let line = inds |> List.fold (fun acc x -> string candidate[random.Next(l)] + acc) "\n"
    
    line