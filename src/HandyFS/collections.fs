///Modules containing functions for working with collections
[<AutoOpen>]
module Collections

    open System
    open System.Collections
    open System.Collections.Generic
    open System.Collections.Specialized

    ///Contains extensions to the standard List module
    [<RequireQualifiedAccess>]
    module List = 

        ///True if a list contains an item
        let contains item = 
            List.exists ((=) item)

        ///True if two lists contain the same items, regardless of order
        let same list1 list2 = 
            if (List.length list1) <> (List.length list2) then
                false
            else
                list1
                |> List.exists (fun item ->
                        list2
                        |> contains item
                        |> not
                    )
                |> not

        ///True if two lists are not the same
        let different list1 list2 = 
            same list1 list2
            |> not

        ///Gets a list of the items which appear in both lists
        let intersect list1 list2 = 
            list1
            |> List.filter (fun item -> contains item list2)

        ///Converts a list of key/value tuples to a NameValueCollection
        let toNvc list =
            
            let rec populate (nvc : NameValueCollection) list = 
                match list with
                | [] -> nvc
                | (key, value) :: rest ->

                    nvc.Add (key, value)

                    populate nvc rest

            populate 
            <| (NameValueCollection ()) 
            <| list

        ///Converts a list of key/value pairs to a mutable dictionary. F#' dict function creates a read-only dictionary.
        let toDictionary<'k, 'v when 'k : equality> (list : ('k * 'v) list) = 
            Dictionary<'k, 'v> (dict list)

    [<RequireQualifiedAccess>]
    module Array =

        ///Gets the item at the head of the array
        let head array = 
            if (Array.isEmpty array) then
                raise (InvalidOperationException ())
            else
                Array.get array 0

    ///Functions for working with IEnumerable and enumerators
    [<RequireQualifiedAccess>]
    module Enumerable = 

        ///Converts an IEnumerable to a sequence
        let toSeq<'a> (enumerable : IEnumerable) =

            let collect (enumerator : IEnumerator) = 
                seq {
                    while enumerator.MoveNext () do
                      yield enumerator.Current :?> 'a  
                }

            collect (enumerable.GetEnumerator ())

        ///Converts an IEnumerable to a list
        let toList<'a> = 
            toSeq<'a> >> Seq.toList

    ///Functions for working with NameValueCollections
    [<RequireQualifiedAccess>]
    module NameValueCollection =

        ///Converts a NameValueCollection to a list of key/value tuples
        let toList (nvc : NameValueCollection) = 
            nvc.AllKeys
            |> Array.toList
            |> List.map (fun key -> (key, nvc.[key]))