namespace DataBlocks

open FSharpPlus

type Json =
    | JsObject of Map<string, Json>
    | JsArray of Json list
    | JsFloat of float
    | JsInteger of int
    | JsBoolean of bool
    | JsString of string
    | JsNull


module Json =

    let private spreadApply2 fab a b arg1 arg2 = fab (a arg1 arg2) (b arg1 arg2)


    type SingleError = SingleError of id : string * error : string


    type DecodeError = 
        | Single of SingleError
        | Multiple of SingleError list with
            static member (+) ( e1 , e2 ) =
                match ( e1 , e2 ) with
                | ( Single e1 , Single e2) -> Multiple [e1; e2]
                | ( Single e , Multiple es ) -> Multiple (e :: es)
                | ( Multiple es , Single e ) -> Multiple (es @ [e])
                | ( Multiple es1 , Multiple es2 ) -> Multiple (es1 @ es2)
            static member get_Zero() = Multiple []            


    type Decoder<'a> = private {
        id : string
        decode : (Json -> string -> Result<'a, DecodeError>)
        }


    let decode decoder json = decoder.decode json decoder.id


    module Decoder =

        let create id decode = { id = id; decode = decode }


        let error id msg = Single (SingleError ( id , msg ))


        let succeed a = create "" (fun _ _ -> Ok a)


        let fail msg = create "" (fun _ id -> Error (error id msg))


        let fromResult result = create "" (fun _ id -> Result.mapError (error id) result)


        let (<?>) decoder newId = create newId decoder.decode


        let string = 
            create 
                "" 
                (fun json id -> 
                    match json with 
                    | JsString s -> Ok s 
                    | _ -> error id "Expected a string value" |> Error
                )


        let int = 
            create 
                "" 
                (fun json id -> 
                    match json with 
                    | JsInteger i -> Ok i
                    | _ -> error id "Expected an integer value" |> Error
                )


        let float = 
            create 
                "" 
                (fun json id -> 
                    match json with 
                    | JsInteger i -> Ok (float i)
                    | JsFloat f -> Ok f
                    | _ -> error id "Expected a numeric value" |> Error
                )


        let bool =
            create 
                ""
                (fun json id ->
                    match json with
                    | JsBoolean b -> Ok b
                    | _ -> error id "Expected a boolean value" |> Error
                )


        let nullable decoder =
            create 
                ""
                (fun json id ->
                    match json with
                    | JsNull -> Ok None
                    | _ -> decode (decoder <?> id) json |> Result.map Some
                )


        let array decoder =
            create
                ""
                (fun json id ->
                    match json with
                    | JsArray xs ->
                        let results = xs |> List.mapi (fun i x -> decode (decoder <?> (sprintf "%s[%d]" id i)) x)
                        let errors = List.collect (function Error e -> [e] | _ -> []) results
                        if not (List.isEmpty errors) then
                            sum errors |> Error
                        else
                            sequence results
                    | _ -> error id "Expected an array" |> Error
                )

        
        let private property noneResult fieldName decoder =
            create
                ""
                (fun json id ->
                    let fieldId = 
                        if id = "" then fieldName
                        else sprintf "%s.%s" id fieldName
                    match json with
                    | JsObject properties -> 
                        match Map.tryFind fieldName properties with
                        | Some x -> decode (decoder <?> fieldId) x
                        | None -> noneResult fieldId
                    | _ -> error id "Expected an object" |> Error
                )

        
        let required fieldName = property (fun id -> (Error (error id "Value is required"))) fieldName

        
        let optional fieldName = nullable >> property (konst (Ok None)) fieldName
            
    type Decoder<'a> with

        static member Return (x : 'a) = Decoder.create "" (fun _ _ -> Ok x)

        static member (<*>) ( dfa , da ) : Decoder<'b> =
            Decoder.create
                dfa.id
                (fun json id ->
                    let result1 = dfa.decode json id
                    let result2 = da.decode json id
                    match ( result1 , result2 ) with
                    | ( Error e1 , Error e2 ) -> Error (e1 ++ e2)
                    | ( _ , Error e2 ) -> Error e2
                    | ( Error e1 , _ ) -> Error e1
                    | ( Ok fa, Ok a ) -> Ok (fa a)
                )

        static member (>>=) ( d : Decoder<'a> , f : 'a -> Decoder<'b> ) : Decoder<'b> =
            Decoder.create
                d.id
                (fun json id ->
                    monad {
                        let! a = d.decode json id
                        let next = f a
                        return! next.decode json id
                    }
                )


        static member (<|>) ( d1 , d2 ) =
            Decoder.create
                d1.id
                (fun json id ->
                    match d1.decode json id with
                    | Error _ -> d2.decode json id
                    | x -> x
                )