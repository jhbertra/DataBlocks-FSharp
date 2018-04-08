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


type SingleError = SingleError of id : string * error : string


type JsonError = 
    | Single of SingleError
    | Multiple of SingleError list


type JsonDecoder<'a> = {
        id : string
        decode : (Json -> string -> Result<'a, JsonError>)
        }


type JsonEncoder<'a> = JsonEncoder of ('a -> Json)


type Json<'a> = {
    decoder : JsonDecoder<'a>
    encoder : JsonEncoder<'a>
    }


type JsonError with
    static member (+) ( e1 , e2 ) =
        match ( e1 , e2 ) with
        | ( Single e1 , Single e2) -> Multiple [e1; e2]
        | ( Single e , Multiple es ) -> Multiple (e :: es)
        | ( Multiple es , Single e ) -> Multiple (es @ [e])
        | ( Multiple es1 , Multiple es2 ) -> Multiple (es1 @ es2)
    static member get_Zero() = Multiple []


module Json =

    let private spreadApply2 fab a b arg1 arg2 = fab (a arg1 arg2) (b arg1 arg2)


    let runDecoder decoder json = decoder.decode json decoder.id


    let runEncoder (JsonEncoder encoder) data = encoder data


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
                    | _ -> runDecoder (decoder <?> id) json |> Result.map Some
                )


        let array decoder =
            create
                ""
                (fun json id ->
                    match json with
                    | JsArray xs ->
                        let results = xs |> List.mapi (fun i x -> runDecoder (decoder <?> (sprintf "%s[%d]" id i)) x)
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
                        | Some x -> runDecoder (decoder <?> fieldId) x
                        | None -> noneResult fieldId
                    | _ -> error id "Expected an object" |> Error
                )

        
        let required fieldName = property (fun id -> (Error (error id "Value is required"))) fieldName

        
        let optional fieldName = nullable >> property (konst (Ok None)) fieldName


    module Encoder =

        let string = JsonEncoder JsString


        let int = JsonEncoder JsInteger


        let float = JsonEncoder JsFloat


        let bool = JsonEncoder JsBoolean


        let nullable (JsonEncoder encoder) = 
            JsonEncoder
              ( function
                | Some x -> encoder x
                | None -> JsNull
              )


        let array (JsonEncoder encoder) = 
            JsonEncoder (fun x -> List.map encoder x |> JsArray)


        let divide splitA (JsonEncoder encoderB) (JsonEncoder encoderC) =
            JsonEncoder
              (fun a ->
                let ( b , c ) = splitA a
                let result1 = encoderB b
                let result2 = encoderC c
                match ( result1 , result2 ) with
                | ( JsObject map1, JsObject map2 ) ->
                    Map.fold (fun acc key value -> Map.add key value acc) map1 map2
                    |> JsObject
                | ( JsObject map, json ) | ( json , JsObject map ) ->
                    Map.add (sprintf "%d" (Map.count map)) json map |> JsObject
                | ( json1, json2 ) ->
                    JsObject (Map [("1", json1); ("2", json2)])
              )


        let object<'a> : JsonEncoder<'a> = JsonEncoder (konst (JsObject Map.empty))


        let property fieldName getter (JsonEncoder encoder) chainEncoder =
            divide
                (fun a -> ( a , getter a ))
                chainEncoder
                (JsonEncoder (encoder >> (flip (Map.add fieldName)) Map.empty >> JsObject))



type JsonDecoder<'a> with

    static member Return (x : 'a) = Json.Decoder.create "" (fun _ _ -> Ok x)

    static member (<*>) ( dfa , da ) : JsonDecoder<'b> =
        Json.Decoder.create
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

    static member (>>=) ( d : JsonDecoder<'a> , f : 'a -> JsonDecoder<'b> ) : JsonDecoder<'b> =
        Json.Decoder.create
            d.id
            (fun json id ->
                monad {
                    let! a = d.decode json id
                    let next = f a
                    return! next.decode json id
                }
            )


    static member (<|>) ( d1 , d2 ) =
        Json.Decoder.create
            d1.id
            (fun json id ->
                match d1.decode json id with
                | Error _ -> d2.decode json id
                | x -> x
            )        


type JsonEncoder<'a> with

    static member Contramap (JsonEncoder f : JsonEncoder<'a>, fba : 'b -> 'a) : JsonEncoder<'b> =
        contramap fba f |> JsonEncoder