module Railway

type Result<'TSuccess,'TFailure> = 
    | Success of 'TSuccess
    | Failure of 'TFailure

type Request = { name : string; email : string }

let validate1 input = 
    if input.name = "" then Failure "Name must not be blank"
    else Success input

let validate2 input = 
    if input.name.Length > 50 then Failure "Name must not be longer than 50 characters"
    else Success input

let validate3 input = 
    if input.email = "" then Failure "Email must no be blank"
    else Success input


let validateInput input = 
    if input.name = "" then Failure "Name must not be blank"
    else if input.email = "" then Failure "Email must not be blank"
    else Success input

let bind switchFunction =
    function
    | Success s -> switchFunction s
    | Failure f -> Failure f

let combinedValidation = 
    let validate2' = bind validate2
    let validate3' = bind validate3
    validate1 >> validate2' >> validate3'

let combinedValidation2 = 
    validate1
    >> bind validate2
    >> bind validate3

let (>>=) twoTrackInput switchFunction =
    bind switchFunction twoTrackInput

let combinedValidation3 x = 
    x
    |> validate1
    >>= validate2 
    >>= validate3

let (>=>) switch1 switch2 x =
    match switch1 x with
    | Success s -> switch2 s
    | Failure f -> Failure f

let combinedValidation4 = 
    validate1
    >=> validate2
    >=> validate3

let (>==>) switch1 switch2 = 
    switch1 >> (bind switch2)

let canonicalizeEmail input = 
    { input with email = input.email.Trim().ToLower() }


// Turn a regular function into
// a switch
let switch f x =
    f x |> Success

let usecase1 = 
    validate1
    >=> validate2
    >=> validate3
    >=> switch canonicalizeEmail

// map
// Take a one track function and
// turn it into a two track function 
// allow it to apply to a two track input
// ---> ------>
// ---> ......>
let map oneTrackFunction twoTrackInput = 
    match twoTrackInput with
    | Success s -> Success (oneTrackFunction s)
    | Failure f -> Failure f

// Use case with map
let usecase2 = 
    validate1
    >=> validate2
    >> map canonicalizeEmail // Notice that we use regular function composition

// Note: >> map f  is equivalent to >=> switch f


// tee
// apply f to x and return x
// Why? Useful for side-effects that we do not 
// want interfering with the function flow
let tee f x =
    f x |> ignore
    x

let updateDatabase input = 
    ()

let usecase3 = 
    validate1
    >=> validate2
    >=> switch (tee updateDatabase)