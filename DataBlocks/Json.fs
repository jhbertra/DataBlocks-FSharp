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


type DecodeError = 
    | Single of SingleError
    | Multiple of SingleError list


type JsonDecoder<'a> = {
        id : string
        decode : (Json -> string -> Result<'a, DecodeError>)
        }


type JsonEncoder<'a> = JsonEncoder of ('a -> Json)


type JsonBlock<'a, 'b> = private {
    decoder : JsonDecoder<'a>
    encoder : JsonEncoder<'b>
    }


type JsonBlock<'a> = Fixed of JsonBlock<'a, 'a>


type DecodeError with
    static member (+) ( e1 , e2 ) =
        match ( e1 , e2 ) with
        | ( Single e1 , Single e2) -> Multiple [e1; e2]
        | ( Single e , Multiple es ) -> Multiple (e :: es)
        | ( Multiple es , Single e ) -> Multiple (es @ [e])
        | ( Multiple es1 , Multiple es2 ) -> Multiple (es1 @ es2)
    static member get_Zero() = Multiple []


module Json =

    let runDecoder decoder json = decoder.decode json decoder.id


    let runEncoder (JsonEncoder encoder) data = encoder data


    let decode (Fixed jsonBlock) = runDecoder jsonBlock.decoder


    let encode (Fixed jsonBlock) = runEncoder jsonBlock.encoder


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
            | Failure ( msg , _ , _ ) -> Result.Error (Single (SingleError ( "" , msg )))


    let decodeString jsonBlock = Parser.parse >=> decode jsonBlock


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


        let map f da = 
            create
                da.id
                    (fun json id ->
                        monad {
                            let! a = da.decode json id
                            return f a 
                        }
                    )


        let apply dfa da = 
            create
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


        let bind f d =
            create
                d.id
                (fun json id ->
                    monad {
                        let! a = d.decode json id
                        let next = f a
                        return! next.decode json id
                    }
                )


        let alternate d1 d2 =
            create
                d1.id
                (fun json id ->
                    match d1.decode json id with
                    | Error _ -> d2.decode json id
                    | x -> x
                )


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


        let choose splitA (JsonEncoder encoderB) (JsonEncoder encoderC) =
            JsonEncoder
                (fun a ->
                    match splitA a with
                    | Choice1Of2 b -> encoderB b
                    | Choice2Of2 c -> encoderC c
                )                


        let switch<'a> : JsonEncoder<'a> = JsonEncoder (konst (JsObject Map.empty))


        let choice getter encoder chainEncoder =
            choose
                (fun a ->  getter a |> Option.map Choice2Of2 |> Option.defaultValue (Choice1Of2 a))
                chainEncoder
                encoder


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


        let contramap f (JsonEncoder e) = contramap f e |> JsonEncoder            


    let create decoder encoder = Fixed {
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


    let switch<'a> : JsonBlock<'a> = Fixed { decoder = Decoder.fail "No choices in switch"; encoder = Encoder.switch }


    let choice getter wrapper (Fixed jsonBlock) (Fixed chainBlock) =
        { decoder = Decoder.alternate (Decoder.map wrapper jsonBlock.decoder) chainBlock.decoder
          encoder = Encoder.choice getter jsonBlock.encoder chainBlock.encoder
        } |> Fixed


    let objectStart f = { decoder = Decoder.succeed f; encoder = Encoder.object }
    let ``{`` = objectStart

    
    let required fieldName (getter : 'c -> 'a) (Fixed json) (chainJson : JsonBlock<('a -> 'b), 'c>) : JsonBlock<'b, 'c> =
        { decoder = Decoder.apply chainJson.decoder (Decoder.required fieldName json.decoder)
          encoder = Encoder.property fieldName getter json.encoder chainJson.encoder
        }

    
    let optional fieldName (getter : 'c -> 'a option) (Fixed json) (chainJson : JsonBlock<('a option -> 'b), 'c>) : JsonBlock<'b, 'c> =
        { decoder = Decoder.apply chainJson.decoder (Decoder.optional fieldName json.decoder)
          encoder = Encoder.property fieldName getter (Encoder.nullable json.encoder) chainJson.encoder
        }


    let objectEnd = Fixed
    let ``}`` = objectEnd


    let invmap f g (Fixed json) = Fixed {
        decoder = Decoder.map f json.decoder
        encoder = Encoder.contramap g json.encoder
    }


    let invbind f g (Fixed json) = Fixed {
        decoder = Decoder.bind (f >> Decoder.fromResult) json.decoder
        encoder = Encoder.contramap g json.encoder
    }


    let modify jsonBlock f = decode jsonBlock >> map (f >> encode jsonBlock)


    let operate jsonBlock f = decode jsonBlock >=> f >=> (encode jsonBlock >> Ok)


type JsonDecoder<'a> with

    static member Return (x : 'a) = Json.Decoder.succeed x

    static member (<*>) ( dfa , da ) : JsonDecoder<'b> = Json.Decoder.apply dfa da

    static member (>>=) ( d : JsonDecoder<'a> , f : 'a -> JsonDecoder<'b> ) : JsonDecoder<'b> =
        Json.Decoder.bind f d

    static member (<|>) ( d1 , d2 ) =
        Json.Decoder.alternate d1 d2


type JsonEncoder<'a> with

    static member Contramap (e : JsonEncoder<'a>, f : 'b -> 'a) : JsonEncoder<'b> =
        Json.Encoder.contramap f e
