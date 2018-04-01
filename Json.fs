namespace FsSerialize

open FsEssentials
open FsEssentials.Prelude

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


    type DecodeError = {
        id : string
        error : string
        children : DecodeError list
        }


    type Decoder<'a> = {
        id : string
        decode : (Json -> string -> Result<'a, DecodeError>)
        }


    let decode decoder json = decoder.decode json decoder.id


    module Decoder =

        let create id decode = { id = id; decode = decode }


        let error id msg = { id = id; error = msg; children = [] }


        let parentError id msg children = { id = id; error = msg; children = children }


        let succeed a = create "{}" (fun _ _ -> Ok a)


        let fail msg = create "{}" (fun _ id -> Error (error id msg))


        let (>>=) d f =
            create
                d.id
                (fun json id ->
                    Result.validate {
                        let! a = d.decode json id
                        let next = f a
                        return! next.decode json id
                    }
                )


        let map f d = d >>= (f >> succeed)


        let (<*>) dfa da =
            create
                dfa.id
                (fun json id ->
                    let result1 = dfa.decode json id
                    let result2 = da.decode json da.id
                    match ( result1 , result2 ) with
                    | ( Error e1 , Error e2 ) ->
                        Error { e1 with children = e1.children @ [e2] }          
                    | ( _ , Error e2 ) -> Error (parentError id "Unable to decode children" [e2])
                    | ( Error e1 , _ ) -> Error e1
                    | ( Ok fa, Ok a ) -> Ok (fa a)
                )


        let (<^>) fa da = succeed fa <*> da


        let (<|>) d1 d2 =
            create d1.id (spreadApply2 Result.(<|>) d1.decode d2.decode)


        let (<?>) decoder newId = create newId decoder.decode


        let string = 
            create 
                "{}" 
                (fun json id -> 
                    match json with 
                    | JsString s -> Ok s 
                    | _ -> error id "Expected a string value" |> Error
                )


        let int = 
            create 
                "{}" 
                (fun json id -> 
                    match json with 
                    | JsInteger i -> Ok i
                    | _ -> error id "Expected an integer value" |> Error
                )


        let float = 
            create 
                "{}" 
                (fun json id -> 
                    match json with 
                    | JsInteger i -> Ok (float i)
                    | JsFloat f -> Ok f
                    | _ -> error id "Expected a numeric value" |> Error
                )


        let bool =
            create 
                "{}"
                (fun json id ->
                    match json with
                    | JsBoolean b -> Ok b
                    | _ -> error id "Expected a boolean value" |> Error
                )


        let nullable decoder =
            create 
                "{}"
                (fun json id ->
                    match json with
                    | JsNull -> Ok None
                    | _ -> decode (decoder <?> id) json |> Result.map Some
                )


        let array decoder =
            create
                "[]"
                (fun json id ->
                    match json with
                    | JsArray xs ->
                        xs
                        |> List.mapi (fun i x -> decode (decoder <?> (sprintf "[%d]" i)) x)
                        |> Result.sequence
                        |> Result.mapError (parentError id "Unable to decode array")
                    | _ -> error id "Expected an array" |> Error
                )

        
        let private property noneResult decoder fieldName =
            create
                "{}"
                (fun json id ->
                    match json with
                    | JsObject properties -> 
                        match Map.tryFind fieldName properties with
                        | Some x -> decode (decoder <?> fieldName) x
                        | None -> noneResult id
                    | _ -> error id "Expected an object" |> Error
                )

        
        let required decoder fieldName =
            property 
                (fun id ->
                    Error (parentError id "Unable to decode object" [error fieldName "Value is required"]) 
                )
                decoder
                fieldName

        
        let optional decoder = property (constant (Ok None)) (nullable decoder)