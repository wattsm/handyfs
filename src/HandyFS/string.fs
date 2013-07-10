///Extensions to the standard String module
[<RequireQualifiedAccess>]
module String

    open System

    ///Converts a string to lower case
    let lower (str : string) = 
        str.ToLower ()

    ///Converts a string to upper case
    let upper (str : string) = 
        str.ToUpper ()

    ///Trims whitespace from a string
    let trim (str : string) = 
        str.Trim ()

    ///True if the two strings are the same, ignoring case
    let same (str1 : string) (str2 : string) = 
        (String.Compare (str1, str2, true) = 0)

    ///Splits a string on a given delimiter, removing empty entries
    let split (delimiter : string) (str : string)  = 
        if (String.IsNullOrWhiteSpace str) then
            []
        else
            let chars = delimiter.ToCharArray ()
            in 
                str.Split (chars, StringSplitOptions.RemoveEmptyEntries)
                |> List.ofArray