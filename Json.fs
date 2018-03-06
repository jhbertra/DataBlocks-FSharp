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

    type DecodeError = 
        | Single of id : string * error : string
        | Aggregate of DecodeError list


    type Decoder<'a> = private Decoder of id : string * decode : (Json -> string -> Result<'a, DecodeError>)


    let decode (Decoder (id , decode)) json = decode json id


    module Decoder =

        let create id decode = Decoder ( id , decode )

        
        let getId (Decoder ( id ,  _ )) = id


        let getDecode (Decoder ( _ ,  decode )) = decode


        let succeed a = create "root" (fun _ _ -> Ok a)


        let (>>=) (Decoder ( id , decode )) f =
            let (>>=) = Result.(>>=)
            create id (fun json id -> decode json id >>= fun a -> (f a |> getDecode) json id)


        let map f d = d >>= (f >> succeed)


        let (<*>) (Decoder ( id, dfa )) (Decoder ( _ , da )) =
            create id (spreadApply2 Result.(<*>) dfa da)


        let (<^>) fa da = succeed fa <*> da


        let (<|>) (Decoder ( id , da1 )) (Decoder ( _ , da2 )) =
            create id (spreadApply2 Result.(<|>) da1 da2)


        let (<?>) (Decoder (_ , decode)) newId = create newId decode


        let string = 
            create 
                "root" 
                (fun json id -> 
                    match json with 
                    | JsString s -> Ok s 
                    | _ -> Single ( id , "Expected a string value" ) |> Error
                )


        let int = 
            create 
                "root" 
                (fun json id -> 
                    match json with 
                    | JsInteger i -> Ok i
                    | _ -> Single ( id , "Expected an integer value" ) |> Error
                )


        let float = 
            create 
                "root" 
                (fun json id -> 
                    match json with 
                    | JsInteger i -> Ok (float i)
                    | JsFloat f -> Ok f
                    | _ -> Single ( id , "Expected a numeric value" ) |> Error
                )


        let bool =
            create 
                "root"
                (fun json id ->
                    match json with
                    | JsBoolean b -> Ok b
                    | _ -> Single ( id , "Expected a boolean value" ) |> Error
                )


        let nullable decoder =
            create 
                "root"
                (fun json _ ->
                    match json with
                    | JsNull -> Ok None
                    | _ -> decode decoder json |> Result.map Some
                )


        let array decoder =
            create
                "root"
                (fun json id ->
                    match json with
                    | JsArray xs ->
                        xs
                        |> List.mapi (fun i x -> decode (decoder <?> (sprintf "%s[%d]" id i)) x)
                        |> Result.sequence
                    | _ -> Single ( id , "Expected an array" ) |> Error
                )

        
        let private property noneResult fieldName decoder =
            create
                "root"
                (fun json id ->
                    let fieldId = sprintf "%s.%s" id fieldName
                    let decoder = decoder <?> fieldId
                    match json with
                    | JsObject properties -> 
                        match Map.tryFind fieldName properties with
                        | Some x -> decode decoder x
                        | None -> noneResult fieldId
                    | _ -> Single ( id , "Expected an object" ) |> Error
                )

        
        let required fieldName = property (fun id -> Single ( id , "Value is required" ) |> Error) fieldName

        
        let optional fieldName = property (constant (Ok None)) fieldName