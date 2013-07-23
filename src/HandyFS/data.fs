///Contains types and functions for working with generic data structures
module HandyFS.Data

    open System

    ///Type annotation defining a document as a list of facets
    type Document = Facet list    

    ///Union describing the different facets of a document
    and Facet = 
        | Annotation of ValueInfo
        | Attribute of ValueInfo        
        | Child of ChildInfo
        | Collection of CollectionInfo

    ///Record describing a child document
    and ChildInfo = {
        Name : String;
        Document : Document;
    }

    ///Record describing a collection 
    and CollectionInfo = {
        Name : String;
        Items : Items;
    }

    ///Union describing collection items
    and Items = 
        | Children of Document list
        | Values of obj list

    ///Record describing a value
    and ValueInfo = {
        Name : String;
        Value : obj option;
    }

    ///Module containing functions for building documents
    [<AutoOpen>]
    module Build = 

        ///Boxes an optional value
        let private boxOption opt = 
            match opt with
            | Some x -> Some (box x)
            | _ -> None

        ///Creates an empty document
        let document () = 
            List.empty<Facet>

        ///Adds an attribute to a document
        let addAttribute name value doc = 
            (Attribute { Name = name; Value = (boxOption value); }) :: doc

        ////Adds an annotation to a document
        let addAnnotation name value doc = 
            (Annotation { Name = name; Value = (boxOption value); }) :: doc

        ///Adds a child to a document
        let addChild name child parent = 
            (Child { Name = name; Document = child; }) :: parent

        ///Adds a collection to a document
        let addCollection name items doc = 
            (Collection { Name = name; Items = items; }) :: doc

        ///Adds a value collection to a document
        let addValueCollection name values doc = 
            
            let values' = 
                values 
                |> List.map box

            (Collection { Name = name; Items = (Values values'); }) :: doc

        ///Adds a document collection to a document
        let addDocumentCollection name children parent = 
            (Collection { Name = name; Items = (Children children); }) :: parent

    ///Module containing functions for querying documents
    [<AutoOpen>]
    module Query = 

        ///Gets the wrapped value from an optional value
        let private getValue info = 
            match info with
            | Some info' -> info'.Value
            | _ -> None

        ///Casts an optional value or returns the default if none
        let private castAs<'a> (info : obj option) = 
            match info with
            | Some value' -> value' :?> 'a
            | _ -> Unchecked.defaultof<'a>

        ///Gets an attribute by name
        let private getAttributeInfo name = 
            List.tryPick (fun facet ->
                match facet with
                | Attribute info -> 
                    if (String.same info.Name name) then
                        Some info
                    else
                        None
                | _ -> None
            )

        ///Gets an annotation by name
        let private getAnnotationInfo name = 
            List.tryPick (fun facet ->
                match facet with
                | Annotation info -> 
                    if (String.same info.Name name) then
                        Some info
                    else
                        None
                | _ -> None
            )

        ///Gets the value of an attribute by name
        let getAttribute name = 
           getAttributeInfo name >> getValue

        ///True if a document has an attribute with a given name
        let hasAttribute name = 
            getAttributeInfo name >> Option.isSome

        ///Gets an attribute value by name and casts it
        let getAttributeAs<'a> name = 
            getAttribute name >> castAs<'a>

        ///Gets an annotation by name
        let getAnnotation name = 
            getAnnotationInfo name >> getValue

        ///True if a document has an annotation with a given name
        let hasAnnotation name = 
            getAnnotationInfo name >> Option.isSome

        ///Gets an annotation by name and casts it
        let getAnnotationAs<'a> name = 
            getAnnotation name >> castAs<'a>

        ///Gets a child by name
        let getChild name = 
            List.tryPick (fun facet ->
                match facet with
                | Child info -> 
                    if (String.same info.Name name) then
                        Some info.Document
                    else
                        None
                | _ -> None
            )

        ///True if a document has a child with a given name
        let hasChild name = 
            getChild name >> Option.isSome

        ///Gets the items of a collection by name
        let getCollection name = 
            List.tryPick (fun facet ->
                match facet with
                | Collection info -> 
                    if (String.same info.Name name) then
                        Some info.Items
                    else
                        None
                | _ -> None
            )

        ///Gets the items of a value collection by name
        let getValueCollection name doc = 
            match (getCollection name doc) with
            | Some (Values items) -> Some items
            | _ -> None    
            
        ///Gets the items of a document collection by name
        let getDocumentCollection name doc = 
            match (getCollection name doc) with
            | Some (Children docs) -> Some docs
            | _ -> None        

        ///True if a document has a collection with a given name
        let hasCollection name = 
            getCollection name >> Option.isSome

        ///True if a document has a value collection with a given name
        let hasValueCollection name doc = 
            match (getCollection name doc) with
            | Some (Values _) -> true
            | _ -> false

        ///True if a document has a document collection with a given name
        let hasDocumentCollection name doc = 
            match (getCollection name doc) with
            | Some (Children _) -> true
            | _ -> false
