namespace FsSerialize.Test


open Xunit
open FsUnit.Xunit

open FSharpPlus
open FSharpPlus.Operators

open FsSerialize
open FsSerialize.Json

module Json =

    let (<?>) = Decoder.(<?>)

    module Decoder =

        let private isOk = function
        | Ok a -> a
        | Error x -> raise(new MatchException("Ok", sprintf "%A" x, null))

        let private isError = function
        | Ok a -> raise(new MatchException("Error", sprintf "%A" a, null))
        | Error x -> x


        // string

        [<Fact>]
        let ``string should decode a string`` () =
            decode (Decoder.string <?> "root") (JsString "Foo") |> isOk |> should equal "Foo"


        [<Fact>]
        let ``string should not decode a boolean`` () =
            decode (Decoder.string <?> "root") (JsBoolean false)
            |> isError |> should equal (Decoder.error "root" "Expected a string value")


        [<Fact>]
        let ``string should not decode an integer`` () =
            decode (Decoder.string <?> "root") (JsInteger 1)
            |> isError |> should equal (Decoder.error "root" "Expected a string value")


        [<Fact>]
        let ``string should not decode an float`` () =
            decode (Decoder.string <?> "root") (JsFloat 1.0)
            |> isError |> should equal (Decoder.error "root" "Expected a string value")


        [<Fact>]
        let ``string should not decode an array`` () =
            decode (Decoder.string <?> "root") (JsArray [JsString "Foo"])
            |> isError |> should equal (Decoder.error "root" "Expected a string value")


        [<Fact>]
        let ``string should not decode an object`` () =
            decode (Decoder.string <?> "root") (JsObject (Map [("Foo", JsString "Foo")]))
            |> isError |> should equal (Decoder.error "root" "Expected a string value")


        [<Fact>]
        let ``string should not decode null`` () =
            decode (Decoder.string <?> "root") JsNull
            |> isError |> should equal (Decoder.error "root" "Expected a string value")


        // bool

        [<Fact>]
        let ``bool should decode a boolean`` () =
            decode (Decoder.bool <?> "root") (JsBoolean false) |> isOk |> should equal false


        [<Fact>]
        let ``bool should not decode a string`` () =
            decode (Decoder.bool <?> "root") (JsString "Foo")
            |> isError |> should equal (Decoder.error "root" "Expected a boolean value")


        [<Fact>]
        let ``bool should not decode an integer`` () =
            decode (Decoder.bool <?> "root") (JsInteger 1)
            |> isError |> should equal (Decoder.error "root" "Expected a boolean value")


        [<Fact>]
        let ``bool should not decode an float`` () =
            decode (Decoder.bool <?> "root") (JsFloat 1.0)
            |> isError |> should equal (Decoder.error "root" "Expected a boolean value")


        [<Fact>]
        let ``bool should not decode an array`` () =
            decode (Decoder.bool <?> "root") (JsArray [JsString "Foo"])
            |> isError |> should equal (Decoder.error "root" "Expected a boolean value")


        [<Fact>]
        let ``bool should not decode an object`` () =
            decode (Decoder.bool <?> "root") (JsObject (Map [("Foo", JsString "Foo")]))
            |> isError |> should equal (Decoder.error "root" "Expected a boolean value")


        [<Fact>]
        let ``bool should not decode null`` () =
            decode (Decoder.bool <?> "root") JsNull
            |> isError |> should equal (Decoder.error "root" "Expected a boolean value")


        // int

        [<Fact>]
        let ``int should decode an integer`` () =
            decode (Decoder.int <?> "root") (JsInteger 1) |> isOk |> should equal 1


        [<Fact>]
        let ``int should not decode a string`` () =
            decode (Decoder.int <?> "root") (JsString "Foo")
            |> isError |> should equal (Decoder.error "root" "Expected an integer value")


        [<Fact>]
        let ``int should not decode a boolean`` () =
            decode (Decoder.int <?> "root") (JsBoolean false)
            |> isError |> should equal (Decoder.error "root" "Expected an integer value")


        [<Fact>]
        let ``int should not decode an float`` () =
            decode (Decoder.int <?> "root") (JsFloat 1.0)
            |> isError |> should equal (Decoder.error "root" "Expected an integer value")


        [<Fact>]
        let ``int should not decode an array`` () =
            decode (Decoder.int <?> "root") (JsArray [JsString "Foo"])
            |> isError |> should equal (Decoder.error "root" "Expected an integer value")


        [<Fact>]
        let ``int should not decode an object`` () =
            decode (Decoder.int <?> "root") (JsObject (Map [("Foo", JsString "Foo")]))
            |> isError |> should equal (Decoder.error "root" "Expected an integer value")


        [<Fact>]
        let ``int should not decode null`` () =
            decode (Decoder.int <?> "root") JsNull
            |> isError |> should equal (Decoder.error "root" "Expected an integer value")


        // float

        [<Fact>]
        let ``float should decode a float`` () =
            decode (Decoder.float <?> "root") (JsFloat 1.0) |> isOk |> should equal 1.0


        [<Fact>]
        let ``float should decode an integer`` () =
            decode (Decoder.float <?> "root") (JsInteger 1) |> isOk |> should equal 1.0


        [<Fact>]
        let ``float should not decode a string`` () =
            decode (Decoder.float <?> "root") (JsString "Foo")
            |> isError |> should equal (Decoder.error "root" "Expected a numeric value")


        [<Fact>]
        let ``float should not decode a boolean`` () =
            decode (Decoder.float <?> "root") (JsBoolean false)
            |> isError |> should equal (Decoder.error "root" "Expected a numeric value")


        [<Fact>]
        let ``float should not decode an array`` () =
            decode (Decoder.float <?> "root") (JsArray [JsString "Foo"])
            |> isError |> should equal (Decoder.error "root" "Expected a numeric value")


        [<Fact>]
        let ``float should not decode an object`` () =
            decode (Decoder.float <?> "root") (JsObject (Map [("Foo", JsString "Foo")]))
            |> isError |> should equal (Decoder.error "root" "Expected a numeric value")


        [<Fact>]
        let ``float should not decode null`` () =
            decode (Decoder.float <?> "root") JsNull
            |> isError |> should equal (Decoder.error "root" "Expected a numeric value")


        // array

        [<Fact>]
        let ``array should not decode an integer`` () =
            decode (Decoder.array (Decoder.succeed ()) <?> "root") (JsInteger 1)
            |> isError |> should equal (Decoder.error "root" "Expected an array")


        [<Fact>]
        let ``array should not decode a string`` () =
            decode (Decoder.array (Decoder.succeed ()) <?> "root") (JsString "Foo")
            |> isError |> should equal (Decoder.error "root" "Expected an array")


        [<Fact>]
        let ``array should not decode a boolean`` () =
            decode (Decoder.array (Decoder.succeed ()) <?> "root") (JsBoolean false)
            |> isError |> should equal (Decoder.error "root" "Expected an array")


        [<Fact>]
        let ``array should not decode an float`` () =
            decode (Decoder.array (Decoder.succeed ()) <?> "root") (JsFloat 1.0)
            |> isError |> should equal (Decoder.error "root" "Expected an array")


        [<Fact>]
        let ``array should not decode an object`` () =
            decode (Decoder.array (Decoder.succeed ()) <?> "root") (JsObject (Map [("Foo", JsString "Foo")]))
            |> isError |> should equal (Decoder.error "root" "Expected an array")


        [<Fact>]
        let ``array should not decode null`` () =
            decode (Decoder.array (Decoder.succeed ()) <?> "root") JsNull
            |> isError |> should equal (Decoder.error "root" "Expected an array")


        [<Fact>]
        let ``array should decode an array`` () =
            decode (Decoder.array Decoder.int <?> "root") (JsArray [JsInteger 1; JsInteger 2])
            |> isOk |> should equal [1; 2]


        [<Fact>]
        let ``array should aggregate failures`` () =
            decode (Decoder.array Decoder.string <?> "root") (JsArray [JsInteger 1; JsInteger 2])
            |> isError 
            |> should
                equal
                ( Decoder.error "root[0]" "Expected a string value"
                  ++ Decoder.error "root[1]" "Expected a string value"
                )


        // nullable

        [<Fact>]
        let ``nullable should decode null`` () =
            decode (Decoder.nullable Decoder.string <?> "root") JsNull |> isOk |> should equal None


        [<Fact>]
        let ``nullable should decode its type`` () =
            decode (Decoder.nullable Decoder.string <?> "root") (JsString "Foo") |> isOk |> should equal (Some "Foo")


        [<Fact>]
        let ``nullable should fail when its decoder does`` () =
            decode (Decoder.nullable Decoder.string <?> "root") (JsInteger 1)
            |> isError |> should equal (Decoder.error "root" "Expected a string value")


        // required

        [<Fact>]
        let ``required should not decode an integer`` () =
            decode (Decoder.required "foo" (Decoder.succeed ()) <?> "root") (JsInteger 1)
            |> isError |> should equal (Decoder.error "root" "Expected an object")


        [<Fact>]
        let ``required should not decode a string`` () =
            decode (Decoder.required "foo" (Decoder.succeed ()) <?> "root") (JsString "Foo")
            |> isError |> should equal (Decoder.error "root" "Expected an object")


        [<Fact>]
        let ``required should not decode a boolean`` () =
            decode (Decoder.required "foo" (Decoder.succeed ()) <?> "root") (JsBoolean false)
            |> isError |> should equal (Decoder.error "root" "Expected an object")


        [<Fact>]
        let ``required should not decode an float`` () =
            decode (Decoder.required "foo" (Decoder.succeed ()) <?> "root") (JsFloat 1.0)
            |> isError |> should equal (Decoder.error "root" "Expected an object")


        [<Fact>]
        let ``required should not decode an array`` () =
            decode (Decoder.required "foo" (Decoder.succeed ()) <?> "root") (JsArray [JsInteger 1; JsInteger 2])
            |> isError |> should equal (Decoder.error "root" "Expected an object")


        [<Fact>]
        let ``required should not decode null`` () =
            decode (Decoder.required "foo" (Decoder.succeed ()) <?> "root") JsNull
            |> isError |> should equal (Decoder.error "root" "Expected an object")


        [<Fact>]
        let ``required should fail when the field is missing`` () =
            decode (Decoder.required "foo" (Decoder.succeed ()) <?> "root") (JsObject (Map [("bar", JsString "bar")]))
            |> isError 
            |> should equal (Decoder.error "root.foo" "Value is required")


        [<Fact>]
        let ``required should fail when its decoder fails`` () =
            decode (Decoder.required "foo" (Decoder.fail "Boom") <?> "root") (JsObject (Map [("foo", JsString "bar")]))
            |> isError 
            |> should equal (Decoder.error "root.foo" "Boom")


        [<Fact>]
        let ``required should succeed when its decoder succeeds`` () =
            decode (Decoder.required "foo" Decoder.string <?> "root") (JsObject (Map [("foo", JsString "bar")]))
            |> isOk |> should equal "bar"


        // optional

        [<Fact>]
        let ``optional should not decode an integer`` () =
            decode (Decoder.optional "foo" (Decoder.succeed ()) <?> "root") (JsInteger 1)
            |> isError |> should equal (Decoder.error "root" "Expected an object")


        [<Fact>]
        let ``optional should not decode a string`` () =
            decode (Decoder.optional "foo" (Decoder.succeed ()) <?> "root") (JsString "Foo")
            |> isError |> should equal (Decoder.error "root" "Expected an object")


        [<Fact>]
        let ``optional should not decode a boolean`` () =
            decode (Decoder.optional "foo" (Decoder.succeed ()) <?> "root") (JsBoolean false)
            |> isError |> should equal (Decoder.error "root" "Expected an object")


        [<Fact>]
        let ``optional should not decode an float`` () =
            decode (Decoder.optional "foo" (Decoder.succeed ()) <?> "root") (JsFloat 1.0)
            |> isError |> should equal (Decoder.error "root" "Expected an object")


        [<Fact>]
        let ``optional should not decode an array`` () =
            decode (Decoder.optional "foo" (Decoder.succeed ()) <?> "root") (JsArray [JsInteger 1; JsInteger 2])
            |> isError |> should equal (Decoder.error "root" "Expected an object")


        [<Fact>]
        let ``optional should not decode null`` () =
            decode (Decoder.optional "foo" (Decoder.succeed ()) <?> "root") JsNull
            |> isError |> should equal (Decoder.error "root" "Expected an object")


        [<Fact>]
        let ``optional should fail when its decoder fails`` () =
            decode (Decoder.optional "foo" (Decoder.fail "Boom") <?> "root") (JsObject (Map [("foo", JsString "bar")]))
            |> isError 
            |> should equal (Decoder.error "root.foo" "Boom")


        [<Fact>]
        let ``optional should succeed when its decoder succeeds`` () =
            decode (Decoder.optional "foo" Decoder.string <?> "root") (JsObject (Map [("foo", JsString "bar")]))
            |> isOk |> should equal (Some "bar")


        [<Fact>]
        let ``optional should decode None when the field is missing`` () =
            decode (Decoder.optional "foo" (Decoder.succeed ()) <?> "root") (JsObject (Map [("bar", JsString "bar")]))
            |> isOk |> should equal None


        // Apply (<*>)

        [<Fact>]
        let ``(<*>) should fail on two failures`` () =
            JsInteger 0
            |> decode (tuple2 <!> Decoder.string <*> Decoder.bool <?> "root")
            |> isError
            |> should
                equal
                ( Decoder.error "root" "Expected a string value"
                  ++ Decoder.error "root" "Expected a boolean value"
                )


        [<Fact>]
        let ``(<*>) should fail on first argument failure`` () =
            JsInteger 0
            |> decode (tuple2 <!> Decoder.string <*> Decoder.int <?> "root")
            |> isError
            |> should equal (Decoder.error "root" "Expected a string value")


        [<Fact>]
        let ``(<*>) should fail on second argument failure`` () =
            JsInteger 0
            |> decode (tuple2 <!> Decoder.int <*> Decoder.bool <?> "root")
            |> isError
            |> should equal (Decoder.error "root" "Expected a boolean value")


        [<Fact>]
        let ``(<*>) should fail on third argument failure`` () =
            JsInteger 0
            |> decode 
              ( tuple3
                <!> Decoder.int
                <*> Decoder.int
                <*> Decoder.bool
                <?> "root"
              )
            |> isError
            |> should equal (Decoder.error "root" "Expected a boolean value")


        [<Fact>]
        let ``(<*>) should apply the function to the successes`` () =
            JsInteger 0
            |> decode (tuple2 <!> Decoder.int <*> Decoder.int <?> "root")
            |> isOk |> should equal ( 0 , 0 )


        // Choose (<|>)


        [<Fact>]
        let ``(<|>) should choose the first success`` () =
            decode
              ( ((konst 1) <!> Decoder.int)
                <|> ((konst 2) <!> Decoder.int)
              )
              (JsInteger 0)
            |> isOk |> should equal 1


        [<Fact>]
        let ``(<|>) should skip failures`` () =
            decode
              ( ((konst 1) <!> Decoder.string)
                <|> ((konst 2) <!> Decoder.int)
              )
              (JsInteger 0)
            |> isOk |> should equal 2


        [<Fact>]
        let ``(<|>) should return the last failure`` () =
            decode
              ( ((konst 1) <!> Decoder.bool)
                <|> ((konst 2) <!> Decoder.string)
              )
              (JsInteger 0)
            |> isError |> should equal (Decoder.error "" "Expected a string value")
            

        // Quasi-realistic scenario

        type PositiveInteger =
            PositiveInteger of int
            with static member Create i =
                    if i > 0 then
                        Ok (PositiveInteger i)
                    else
                        Error "Value must be positive"


        type ChildFieldType =
            | Choice1 of int
            | Choice2 of string        


        type SuperAdvancedOptions = {
            childField: ChildFieldType
        }


        let superAdvancedOptions childField = { childField = childField }


        type AdvancedOptions = {
            allowNonsecure : bool
            superAdvanced : SuperAdvancedOptions
        }


        let advancedOptions allowNonsecure superAdvanced = {
            allowNonsecure = allowNonsecure
            superAdvanced = superAdvanced
        }


        type ImaginationConfig = {
            url : string
            connectionLimit : PositiveInteger
            trustedUrls : string list
            email : string option
            advanced : AdvancedOptions
        }


        let imaginationConfig url connectionLimit trustedUrls email advanced = {
            url = url
            connectionLimit = connectionLimit
            trustedUrls = trustedUrls
            email = email
            advanced = advanced
        }

        open Decoder


        let superAdvancedOptionsDecoder =
            superAdvancedOptions
            <!> required "childField" ( (int |> map Choice1) <|> (string |> map Choice2) )


        let advancedOptionsDecoder =
            advancedOptions
            <!> required "allowNonsecure" bool
            <*> required "superAdvanced" superAdvancedOptionsDecoder


        let imaginationConfigDecoder =
            imaginationConfig
            <!> required "url" string
            <*> required "connectionLimit" (int >>= (PositiveInteger.Create >> fromResult))
            <*> required "trustedUrls" (array string)
            <*> optional "email" string
            <*> required "advanced" advancedOptionsDecoder


        [<Fact>]
        let ``The API should suitably handle errors for more than trivial examples`` () =
            decode
                imaginationConfigDecoder
                ( JsObject ( Map
                    [
                        ( "url" , JsInteger 1 )
                        ( "connectionLimit" , JsInteger -1 )
                        ( "email" , JsBoolean false )
                        ( "advanced" , JsObject ( Map
                            [ 
                                ( "allowNonsecure" , JsBoolean false )
                                ( "superAdvanced" , JsObject ( Map [ ( "childField" , JsBoolean true ) ] ) )
                            ])
                      )
                    ])
                )
            |> isError
            |> should
                equal
                ( (Decoder.error "url" "Expected a string value")
                  ++ (Decoder.error "connectionLimit" "Value must be positive")
                  ++ (Decoder.error "trustedUrls" "Value is required")
                  ++ (Decoder.error "email" "Expected a string value")
                  ++ (Decoder.error "advanced.superAdvanced.childField" "Expected a string value")
                )


        [<Fact>]
        let ``The API should suitably handle successes for more than trivial examples`` () =
            decode
                imaginationConfigDecoder
                ( JsObject ( Map
                    [
                        ( "url" , JsString "http://www.google.com" )
                        ( "connectionLimit" , JsInteger 1 )
                        ( "trustedUrls" , JsArray [JsString "url1"] )
                        ( "email" , JsString "asdf@test.com" )
                        ( "advanced" , JsObject ( Map
                            [ 
                                ( "allowNonsecure" , JsBoolean false )
                                ( "superAdvanced" , JsObject ( Map [ ( "childField" , JsInteger 1 ) ] ) )
                            ])
                      )
                    ])
                )
            |> isOk
            |> should
                equal
                { url = "http://www.google.com"
                  connectionLimit = PositiveInteger 1
                  trustedUrls = ["url1"]
                  email = Some "asdf@test.com"
                  advanced =
                    { allowNonsecure = false
                      superAdvanced = { childField = Choice1 1 }
                    }
                }