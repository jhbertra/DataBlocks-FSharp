namespace FsSerialize.Test


open Xunit
open FsUnit.Xunit

open FsEssentials.Prelude
open FsSerialize
open FsSerialize.Json

module Json =

    let (<?>) = Decoder.(<?>)
    let (<^>) = Decoder.(<^>)
    let (<*>) = Decoder.(<*>)
    let (<|>) = Decoder.(<|>)

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
                (Decoder.parentError
                    "root"
                    "Unable to decode array"
                    [
                        Decoder.error "[0]" "Expected a string value"
                        Decoder.error "[1]" "Expected a string value"
                    ]
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
            decode (Decoder.required (Decoder.succeed ()) "foo" <?> "root") (JsInteger 1)
            |> isError |> should equal (Decoder.error "root" "Expected an object")

        [<Fact>]
        let ``required should not decode a string`` () =
            decode (Decoder.required (Decoder.succeed ()) "foo" <?> "root") (JsString "Foo")
            |> isError |> should equal (Decoder.error "root" "Expected an object")

        [<Fact>]
        let ``required should not decode a boolean`` () =
            decode (Decoder.required (Decoder.succeed ()) "foo" <?> "root") (JsBoolean false)
            |> isError |> should equal (Decoder.error "root" "Expected an object")

        [<Fact>]
        let ``required should not decode an float`` () =
            decode (Decoder.required (Decoder.succeed ()) "foo" <?> "root") (JsFloat 1.0)
            |> isError |> should equal (Decoder.error "root" "Expected an object")

        [<Fact>]
        let ``required should not decode an array`` () =
            decode (Decoder.required (Decoder.succeed ()) "foo" <?> "root") (JsArray [JsInteger 1; JsInteger 2])
            |> isError |> should equal (Decoder.error "root" "Expected an object")

        [<Fact>]
        let ``required should not decode null`` () =
            decode (Decoder.required (Decoder.succeed ()) "foo" <?> "root") JsNull
            |> isError |> should equal (Decoder.error "root" "Expected an object")

        [<Fact>]
        let ``required should fail when the field is missing`` () =
            decode (Decoder.required (Decoder.succeed ()) "foo" <?> "root") (JsObject (Map [("bar", JsString "bar")]))
            |> isError 
            |> should
                equal
                (Decoder.parentError
                    "root"
                    "Unable to decode object"
                    [Decoder.error "foo" "Value is required"]
                )

        [<Fact>]
        let ``required should fail when its decoder fails`` () =
            decode (Decoder.required (Decoder.fail "Boom") "foo" <?> "root") (JsObject (Map [("foo", JsString "bar")]))
            |> isError 
            |> should equal (Decoder.error "foo" "Boom")

        [<Fact>]
        let ``required should succeed when its decoder succeeds`` () =
            decode (Decoder.required Decoder.string "foo" <?> "root") (JsObject (Map [("foo", JsString "bar")]))
            |> isOk |> should equal "bar"

        // optional
        
        [<Fact>]
        let ``optional should not decode an integer`` () =
            decode (Decoder.optional (Decoder.succeed ()) "foo" <?> "root") (JsInteger 1)
            |> isError |> should equal (Decoder.error "root" "Expected an object")

        [<Fact>]
        let ``optional should not decode a string`` () =
            decode (Decoder.optional (Decoder.succeed ()) "foo" <?> "root") (JsString "Foo")
            |> isError |> should equal (Decoder.error "root" "Expected an object")

        [<Fact>]
        let ``optional should not decode a boolean`` () =
            decode (Decoder.optional (Decoder.succeed ()) "foo" <?> "root") (JsBoolean false)
            |> isError |> should equal (Decoder.error "root" "Expected an object")

        [<Fact>]
        let ``optional should not decode an float`` () =
            decode (Decoder.optional (Decoder.succeed ()) "foo" <?> "root") (JsFloat 1.0)
            |> isError |> should equal (Decoder.error "root" "Expected an object")

        [<Fact>]
        let ``optional should not decode an array`` () =
            decode (Decoder.optional (Decoder.succeed ()) "foo" <?> "root") (JsArray [JsInteger 1; JsInteger 2])
            |> isError |> should equal (Decoder.error "root" "Expected an object")

        [<Fact>]
        let ``optional should not decode null`` () =
            decode (Decoder.optional (Decoder.succeed ()) "foo" <?> "root") JsNull
            |> isError |> should equal (Decoder.error "root" "Expected an object")

        [<Fact>]
        let ``optional should fail when its decoder fails`` () =
            decode (Decoder.optional (Decoder.fail "Boom") "foo" <?> "root") (JsObject (Map [("foo", JsString "bar")]))
            |> isError 
            |> should equal (Decoder.error "foo" "Boom")

        [<Fact>]
        let ``optional should succeed when its decoder succeeds`` () =
            decode (Decoder.optional Decoder.string "foo" <?> "root") (JsObject (Map [("foo", JsString "bar")]))
            |> isOk |> should equal (Some "bar")

        [<Fact>]
        let ``optional should decode None when the field is missing`` () =
            decode (Decoder.optional (Decoder.succeed ()) "foo" <?> "root") (JsObject (Map [("bar", JsString "bar")]))
            |> isOk |> should equal None

        // Apply (<*>)

        [<Fact>]
        let ``(<*>) should fail on two failures`` () =
            let makeTuple x y = ( x, y )
            JsInteger 0
            |> decode (makeTuple <^> (Decoder.string <?> "1") <*> (Decoder.bool <?> "2") <?> "root")
            |> isError
            |> should
                equal
                (Decoder.parentError
                    "root"
                    "Unable to decode children"
                    [
                        Decoder.error "1" "Expected a string value"
                        Decoder.error "2" "Expected a boolean value"
                    ]
                )

        [<Fact>]
        let ``(<*>) should fail on first argument failure`` () =
            let makeTuple x y = ( x, y )
            JsInteger 0
            |> decode (makeTuple <^> (Decoder.string <?> "1") <*> (Decoder.int <?> "2") <?> "root")
            |> isError
            |> should
                equal
                (Decoder.parentError
                    "root"
                    "Unable to decode children"
                    [Decoder.error "1" "Expected a string value"]
                )

        [<Fact>]
        let ``(<*>) should fail on second argument failure`` () =
            let makeTuple x y = ( x, y )
            JsInteger 0
            |> decode (makeTuple <^> (Decoder.int <?> "1") <*> (Decoder.bool <?> "2") <?> "root")
            |> isError
            |> should
                equal
                (Decoder.parentError
                    "root"
                    "Unable to decode children"
                    [Decoder.error "2" "Expected a boolean value"]
                )

        [<Fact>]
        let ``(<*>) should fail on third argument failure`` () =
            let makeTuple x y z = ( x , y , z )
            JsInteger 0
            |> decode 
              ( makeTuple
                <^> (Decoder.int <?> "1")
                <*> (Decoder.int <?> "2")
                <*> (Decoder.bool <?> "3")
                <?> "root"
              )
            |> isError
            |> should
                equal
                (Decoder.parentError
                    "root"
                    "Unable to decode children"
                    [Decoder.error "3" "Expected a boolean value"]
                )

        [<Fact>]
        let ``(<*>) should apply the function to the successes`` () =
            let makeTuple x y = ( x, y )
            JsInteger 0
            |> decode (makeTuple <^> (Decoder.int <?> "1") <*> (Decoder.int <?> "2") <?> "root")
            |> isOk |> should equal ( 0 , 0 )

        // Choose (<|>)

        let ``(<|>) should choose the first success`` () =
            decode
              ( (Decoder.int |> Decoder.map (constant 1))
                <|> (Decoder.int |> Decoder.map (constant 2))
                <?> "root"
              )
              (JsInteger 0)
            |> isOk |> should equal 1

        let ``(<|>) should skip failures`` () =
            decode
              ( (Decoder.string |> Decoder.map (constant 1))
                <|> (Decoder.int |> Decoder.map (constant 2))
                <?> "root"
              )
              (JsInteger 0)
            |> isOk |> should equal 2

        let ``(<|>) should return the last failure`` () =
            decode
              ( (Decoder.bool |> Decoder.map (constant 1))
                <|> (Decoder.string |> Decoder.map (constant 2))
                <?> "root"
              )
              (JsInteger 0)
            |> isError |> should equal (Decoder.error "2" "Expected a string value")
            