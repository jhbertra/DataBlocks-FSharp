namespace DataBlocks

open Aether
open FSharpPlus

open Core

type Json =
    | JsObject of Map<string, Json>
    | JsArray of Json list
    | JsFloat of float
    | JsInteger of int
    | JsBoolean of bool
    | JsString of string
    | JsNull


module Json =

    let combineJson json1 json2 =
        match ( json1 , json2 ) with
        | ( JsObject map1, JsObject map2 ) ->
            Map.fold (fun acc key value -> Map.add key value acc) map1 map2
            |> JsObject
        | ( JsObject map, json ) | ( json , JsObject map ) ->
            Map.add (sprintf "%d" (Map.count map)) json map |> JsObject
        | ( json1, json2 ) ->
            JsObject (Map [("1", json1); ("2", json2)])

    let jsonEmpty = JsObject Map.empty        

    module Parser =

        open FParsec

        let jnull<'a> : Parser<Json, 'a> = stringReturn "null" JsNull

        let jtrue<'a> : Parser<Json, 'a> = stringReturn "true" (JsBoolean true)

        let jfalse<'a> : Parser<Json, 'a> = stringReturn "false" (JsBoolean false)

        let jnumber<'a> : Parser<Json, 'a> = 
            pfloat 
            |>> (fun n ->
                    if n % 1.0 = 0.0 then
                        JsInteger (int n)
                    else
                        JsFloat n
                )

        let stringLiteral<'a> : Parser<string, 'a> =
            let escape =  anyOf "\"\\/bfnrt"
                          |>> function
                              | 'b' -> "\b"
                              | 'f' -> "\u000C"
                              | 'n' -> "\n"
                              | 'r' -> "\r"
                              | 't' -> "\t"
                              | c   -> string c

            let unicodeEscape =
                /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
                let hex2int c = (int c &&& 15) + (int c >>> 6)*9

                pstring "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                    (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                    |> char |> string
                )

            let escapedCharSnippet = pstring "\\" >>. (escape <|> unicodeEscape)
            let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

            between (pstring "\"") (pstring "\"")
                    (stringsSepBy normalCharSnippet escapedCharSnippet)

        let jstring<'a> : Parser<Json, 'a> = stringLiteral |>> JsString            

        let jvalue, jvalueRef = createParserForwardedToRef<Json, unit>()

        let listBetweenStrings sOpen sClose pElement f =
            between (pstring sOpen) (pstring sClose)
                    (spaces >>. sepBy (pElement .>> spaces) (pstring "," >>. spaces) |>> f)

        let jlist = listBetweenStrings "[" "]" jvalue JsArray

        let keyValue = stringLiteral .>>. (spaces >>. pstring ":" >>. spaces >>. jvalue)   

        let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JsObject)

        do jvalueRef := choice
            [ jobject
              jlist
              jstring
              jnumber
              jtrue
              jfalse
              jnull
            ]
        
        let json = spaces >>. jvalue .>> spaces .>> eof

        
        let parse text =
            match run json text with
            | Success ( result , _ , _ ) -> Result.Ok result
            | Failure ( msg , _ , _ ) -> Result.Error (Single { id = ""; message = msg })


    let decodeString jsonBlock = Parser.parse >=> decode jsonBlock


    module Decoder =

        let create id decode : Decoder<Json, 'a> = decoder id decode


        let succeed a : Decoder<Json, 'a> = Decoder.succeed a


        let fail msg : Decoder<Json, 'a> = Decoder.fail msg


        let fromResult = Decoder.fromResult


        let (<?>) decoder newId = create newId decoder.decode


        let string = 
            create 
                "" 
                (fun id json -> 
                    match json with 
                    | JsString s -> Ok s 
                    | _ -> error id "Expected a string value" |> Error
                )


        let int = 
            create 
                "" 
                (fun id json -> 
                    match json with 
                    | JsInteger i -> Ok i
                    | _ -> error id "Expected an integer value" |> Error
                )


        let float = 
            create 
                "" 
                (fun id json -> 
                    match json with 
                    | JsInteger i -> Ok (float i)
                    | JsFloat f -> Ok f
                    | _ -> error id "Expected a numeric value" |> Error
                )


        let bool =
            create 
                ""
                (fun id json ->
                    match json with
                    | JsBoolean b -> Ok b
                    | _ -> error id "Expected a boolean value" |> Error
                )


        let nullable decoder =
            create 
                ""
                (fun id json ->
                    match json with
                    | JsNull -> Ok None
                    | _ -> runDecoder (decoder <?> id) json |> Result.map Some
                )


        let array decoder =
            create
                ""
                (fun id json ->
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
                (fun id json ->
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

        let encoder f : Encoder<'a, Json> = Encoder f

        let string = encoder JsString


        let int = encoder JsInteger


        let float = encoder JsFloat


        let bool = encoder JsBoolean


        let nullable (Encoder e) = 
            encoder
              ( function
                | Some x -> e x
                | None -> JsNull
              )


        let array (Encoder e) = 
            encoder (fun x -> List.map e x |> JsArray)


        let choose splitA eb ec : Encoder<'a, Json> = Encoder.choose splitA eb ec               


        let switch<'a> : Encoder<'a, Json> = encoder (konst jsonEmpty)


        let choice getter encoder chainEncoder =
            choose
                (fun a ->  getter a |> Option.map Choice2Of2 |> Option.defaultValue (Choice1Of2 a))
                chainEncoder
                encoder


        let divide splitA eb ec =
            Encoder.divide
                combineJson
                splitA
                eb
                ec


        let object<'a> : Encoder<'a, Json> = Encoder (konst jsonEmpty)


        let property fieldName getter (Encoder encoder) chainEncoder =
            divide
                (fun a -> ( a , getter a ))
                chainEncoder
                (Encoder (encoder >> (flip (Map.add fieldName)) Map.empty >> JsObject))


    let create decoder encoder : DataBlock<Json, 'a> = Fixed {
        decoder = decoder
        encoder = encoder
    }


    let (<?>) (Fixed jsonBlock) newId = Fixed { jsonBlock with decoder = { jsonBlock.decoder with id = newId } }


    let string = create Decoder.string Encoder.string


    let int = create Decoder.int Encoder.int


    let float =  create Decoder.float Encoder.float


    let bool = create Decoder.bool Encoder.bool


    let nullable (Fixed jsonBlock) = create (Decoder.nullable jsonBlock.decoder) (Encoder.nullable jsonBlock.encoder)


    let array (Fixed jsonBlock) = create (Decoder.array jsonBlock.decoder) (Encoder.array jsonBlock.encoder)


    let switch<'a> : DataBlock<Json, 'a> = Fixed { decoder = Decoder.fail "No choices in switch"; encoder = Encoder.switch }


    let choice getter wrapper (Fixed jsonBlock) (Fixed chainBlock) =
        { decoder = Decoder.alternate (Decoder.map wrapper jsonBlock.decoder) chainBlock.decoder
          encoder = Encoder.choice getter jsonBlock.encoder chainBlock.encoder
        } |> Fixed


    let ``{`` f = disengage jsonEmpty f

    let required fieldName getter propBlock remainderBlock =
        part 
            combineJson
            getter
            ( Fixed
                { decoder = Decoder.required fieldName (Optic.get blockDecoder propBlock)
                  encoder = (Encoder ((Optic.get blockEncode propBlock) >> (flip (Map.add fieldName)) Map.empty >> JsObject)) })
            remainderBlock

    
    let optional fieldName getter propBlock remainderBlock =
        part 
            combineJson
            getter
            ( Fixed
                { decoder = Decoder.optional fieldName (Optic.get blockDecoder propBlock)
                  encoder = (Encoder ((Optic.get encoderEncode (Encoder.nullable (Optic.get blockEncoder propBlock))) >> (flip (Map.add fieldName)) Map.empty >> JsObject)) })
            remainderBlock

    let ``}`` = engage
