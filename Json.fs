namespace FsEncode

type JsProperty = JsProperty of name : string * value : Json
and JsArray = JsArray of Json list
and JsObject = JsObject of properties : JsProperty list
and Json =
    | JsObject of JsObject
    | JsArray of JsArray
    | JsNumber of float
    | JsBoolean of bool
    | JsString of string


module Json =

    let private flip f x y = f y x


    type DecodeError = private DecodeError of id : string * error : string


    type Decoder<'a> = private Decoder of id : string * description : string * decode : (Json -> DecodeError list -> Result<'a, DecodeError list>)


    let decode (Decoder (id , description , decode)) json = decode json []


    module Decoder =

        let create id description decode =
            Decoder
                ( id
                , description
                , (fun json errors ->
                    match decode json with
                    | None -> DecodeError ( id , sprintf "Expected %s" description ) :: errors |> Error
                    | Some result -> Ok result)
                )

        
        let getId (Decoder (id , _,  _)) = id


        let getDescription (Decoder (_ , description,  _)) = description


        let constant a = Decoder ("root" , "" , (fun _ _ -> Ok a))


        let (>>=) (Decoder (id , description , decode)) f =
            Decoder 
                ( id
                , description
                , fun json errors ->
                    decode json errors
                    |> Result.bind (fun a ->
                        let (Decoder (_ , _ , decode)) = f a
                        decode json errors
                        )
                )


        let map f d = d >>= (f >> constant)


        let (<*>) (Decoder (id1, description1, dfa)) (Decoder (_, description2, da)) = 
            Decoder 
                ( id1
                , match description1 with
                  | "" -> description2
                  | x -> sprintf "%s and %s" x description2
                , fun json errors ->
                    match dfa json errors with
                    | Error errors ->
                        match da json errors with
                        | Error errors -> Error errors
                        | _ -> Error errors
                    | Ok fa ->
                        match da json errors with
                        | Error errors -> Error errors
                        | Ok a -> Ok (fa a)
                )


        let (<|>) (Decoder (id1, description1, da1)) (Decoder (id2, description2, da2)) =
            Decoder
                ( id1
                , match description1 with
                  | "" -> description2
                  | x -> sprintf "%s or %s" x description2
                , fun json errors ->
                    match da1 json errors with
                    | Error errors -> da2 json errors
                    | Ok a -> Ok a
                )


        let (<?>) (Decoder (_ , description , decode)) newId = (Decoder (newId , description , decode))