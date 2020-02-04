﻿// Suppose I have a website where a user is attempting to register an account. I can 
// encapsulate such a request as a record:
open System

type RegistrationAttempt = {username: string; email: string}

// As in:
let reg1 = {username = "neal"; email = "neal.terrell@csulb.edu"}

// Suppose I want to write a function to validate a registration attempt, checking against
// many rules for new accounts. In an OOP language, it might look something like this:

let validateRegistration reg =
    let {username = name; email = em} = reg
    if String.length name = 0 then
        "Missing a username"
    elif not (em.Contains("@")) then
        "Invalid email address"
    else
        "Valid"


// A better return type:
type ValidationResult =
    | Success of RegistrationAttempt
    | Failure of string

// A function can now return a ValidationResult, indicating success or failure. We can also
// start composing our validation functions.
let usernameExists reg =
    if reg.username.Length > 0 then
        Success reg
    else
        Failure "Username must not be blank"
// username: RegistrationAttempt -> ValidationResult

let emailHasAtSign reg = 
    if reg.email.Contains("@") then
        Success reg
    else
        Failure "Email address must contain a @"

let emailHasLocalPart reg =
    if reg.email.IndexOf("@") > 0 then
        Success reg
    else
        Failure "Email address does not have a local-part"



let bind switchFunction twoTrackInput =
    match twoTrackInput with
    | Success s -> switchFunction s
    | Failure f -> Failure f

// Binding is so common that we can introduce an operator to make our life easier.
let (>=>) switch1 switch2 reg =
    bind switch2 (switch1 reg)

let existingAccounts = ["zacharyminott@yahoo.com"; "arking@gmail.com"; "masterofpuppets@gmail.com"; "mistermax@yahoo.com"; "noobprogrammer@msn.com"]

let blacklistDomains = ["mailinator.org"; "throwawaymail.com"]

// Checks to see if the email is unique
let uniqueEmails list reg = 
    if List.contains reg.email list then 
        Failure "This email is already registered"
    else 
        Success reg

// Checks to see if the email exists in the blacklisted domains
let emailNotBlacklisted list reg =
    let domain = reg.email.[(reg.email.IndexOf("@") + 1)..]

    if List.contains domain list then 
        Failure "This email is domain is blacklisted"
    else 
        Success reg 

// Switch function for Single Track Functions
let bypass f reg = 
    f reg |> Success

let (>->) switch1 by reg = 
    bind (bypass by) (switch1 reg)

// Lowercase the email
let lowercaseEmail reg = 
    let newReg = {username = reg.username; email = reg.email.ToLower()}
    newReg

// Helper function to strip characters from a string
let stripChars text (chars:string) =
    Array.fold (
        fun (s:string) c -> s.Replace(c.ToString(),"")
    ) text (chars.ToCharArray()) 

// CANONICALIZE EMAIl
let canonicalizeEmail reg =
    let domain = reg.email.[(reg.email.IndexOf("@") + 1)..]

    let canonicalize email = 
        let leftEmail = reg.email.[0..(reg.email.IndexOf("@") - 1)]
        let stripPeriods = stripChars leftEmail "-."

        if stripPeriods.Contains("+") then 
            let stripAddOp = stripPeriods.[0..(reg.email.IndexOf("+") - 1)]
            let newEmail = stripAddOp + "@gmail.com" 
            let newReg = {username = reg.username; email = newEmail}
            newReg
        else 
            let newEmail = stripPeriods + "@gmail.com" 
            let newReg = {username = reg.username; email = newEmail}
            newReg



    if domain = "gmail.com" then 
        canonicalize domain 
    else
        reg


// VALIDATION FUNCTION
let validate4 = 
    usernameExists
    >=> emailHasAtSign
    >=> emailHasLocalPart
    >-> lowercaseEmail 
    >-> canonicalizeEmail 
    >=> emailNotBlacklisted blacklistDomains
    >=> uniqueEmails existingAccounts

// TEST CASES
let regTest1 = {username = "zack"; email = "neal.terrell@gmail.com"}
let regTest2 = {username = "zimmer"; email = "live+m.an@gmail.com"}
let regTest3 = {username = "zman"; email = "zack.min@yahoo.com"}
let regTest4 = {username = "zap"; email = "liverzap@mailinator.org"}
let regTest5 = {username = "exzackt"; email = "DUHDUHDUH@yahoo.com"}
let regTest6 = {username = ""; email = "Ihavenoemail@email.net"}
let regTest7 = {username = "ihavenolocal"; email = "@yahoo.com"}
let regTest8 = {username = "myemailisregistered"; email = "masterofpuppets@gmail.com"}

validate4 regTest1 |> printfn "\n\nvalidate test 1:\n%A"
validate4 regTest2 |> printfn "\n\nvalidate test 2:\n%A"
validate4 regTest3 |> printfn "\n\nvalidate test 3:\n%A"
validate4 regTest4 |> printfn "\n\nvalidate test 4:\n%A"
validate4 regTest5 |> printfn "\n\nvalidate test 5:\n%A"
validate4 regTest6 |> printfn "\n\nvalidate test 6:\n%A"
validate4 regTest7 |> printfn "\n\nvalidate test 7:\n%A"
validate4 regTest8 |> printfn "\n\nvalidate test 8:\n%A"
    
    
Console.ReadKey() |> ignore