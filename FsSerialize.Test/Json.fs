namespace FsSerialize.Test


open Xunit
open FsUnit.Xunit

open FsSerialize
open FsSerialize.Json

module Json =

    module Decoder =

        let private shouldEqualResult a = sprintf "%A" >> (sprintf "%A" a |> should equal)

        // string

        [<Fact>]
        let ``string should decode a string`` () =
            decode Decoder.string (JsString "Foo") |> shouldEqualResult (Ok "Foo")

        [<Fact>]
        let ``string should not decode a boolean`` () =
            decode Decoder.string (JsBoolean false)
            |> shouldEqualResult (Error (Single ("root" , "Expected a string value")))

        [<Fact>]
        let ``string should not decode an integer`` () =
            decode Decoder.string (JsInteger 1)
            |> shouldEqualResult (Error (Single ("root" , "Expected a string value")))

        [<Fact>]
        let ``string should not decode an float`` () =
            decode Decoder.string (JsFloat 1.0)
            |> shouldEqualResult (Error (Single ("root" , "Expected a string value")))

        [<Fact>]
        let ``string should not decode an array`` () =
            decode Decoder.string (JsArray [JsString "Foo"])
            |> shouldEqualResult (Error (Single ("root" , "Expected a string value")))

        [<Fact>]
        let ``string should not decode an object`` () =
            decode Decoder.string (JsObject (Map [("Foo", JsString "Foo")]))
            |> shouldEqualResult (Error (Single ("root" , "Expected a string value")))

        [<Fact>]
        let ``string should not decode null`` () =
            decode Decoder.string JsNull
            |> shouldEqualResult (Error (Single ("root" , "Expected a string value")))

        // bool

        [<Fact>]
        let ``bool should decode a boolean`` () =
            decode Decoder.bool (JsBoolean false) |> shouldEqualResult (Ok false)

        [<Fact>]
        let ``bool should not decode a string`` () =
            decode Decoder.bool (JsString "Foo")
            |> shouldEqualResult (Error (Single ("root" , "Expected a boolean value")))

        [<Fact>]
        let ``bool should not decode an integer`` () =
            decode Decoder.bool (JsInteger 1)
            |> shouldEqualResult (Error (Single ("root" , "Expected a boolean value")))

        [<Fact>]
        let ``bool should not decode an float`` () =
            decode Decoder.bool (JsFloat 1.0)
            |> shouldEqualResult (Error (Single ("root" , "Expected a boolean value")))

        [<Fact>]
        let ``bool should not decode an array`` () =
            decode Decoder.bool (JsArray [JsString "Foo"])
            |> shouldEqualResult (Error (Single ("root" , "Expected a boolean value")))

        [<Fact>]
        let ``bool should not decode an object`` () =
            decode Decoder.bool (JsObject (Map [("Foo", JsString "Foo")]))
            |> shouldEqualResult (Error (Single ("root" , "Expected a boolean value")))

        [<Fact>]
        let ``bool should not decode null`` () =
            decode Decoder.bool JsNull
            |> shouldEqualResult (Error (Single ("root" , "Expected a boolean value")))

        // int

        [<Fact>]
        let ``int should decode an integer`` () =
            decode Decoder.int (JsInteger 1) |> shouldEqualResult (Ok 1)

        [<Fact>]
        let ``int should not decode a string`` () =
            decode Decoder.int (JsString "Foo")
            |> shouldEqualResult (Error (Single ("root" , "Expected an integer value")))

        [<Fact>]
        let ``int should not decode a boolean`` () =
            decode Decoder.int (JsBoolean false)
            |> shouldEqualResult (Error (Single ("root" , "Expected an integer value")))

        [<Fact>]
        let ``int should not decode an float`` () =
            decode Decoder.int (JsFloat 1.0)
            |> shouldEqualResult (Error (Single ("root" , "Expected an integer value")))

        [<Fact>]
        let ``int should not decode an array`` () =
            decode Decoder.int (JsArray [JsString "Foo"])
            |> shouldEqualResult (Error (Single ("root" , "Expected an integer value")))

        [<Fact>]
        let ``int should not decode an object`` () =
            decode Decoder.int (JsObject (Map [("Foo", JsString "Foo")]))
            |> shouldEqualResult (Error (Single ("root" , "Expected an integer value")))

        [<Fact>]
        let ``int should not decode null`` () =
            decode Decoder.int JsNull
            |> shouldEqualResult (Error (Single ("root" , "Expected an integer value")))

        // float

        [<Fact>]
        let ``float should decode a float`` () =
            decode Decoder.float (JsFloat 1.0) |> shouldEqualResult (Ok 1.0)

        [<Fact>]
        let ``float should decode an integer`` () =
            decode Decoder.float (JsInteger 1) |> shouldEqualResult (Ok 1.0)

        [<Fact>]
        let ``float should not decode a string`` () =
            decode Decoder.float (JsString "Foo")
            |> shouldEqualResult (Error (Single ("root" , "Expected a numeric value")))

        [<Fact>]
        let ``float should not decode a boolean`` () =
            decode Decoder.float (JsBoolean false)
            |> shouldEqualResult (Error (Single ("root" , "Expected a numeric value")))

        [<Fact>]
        let ``float should not decode an array`` () =
            decode Decoder.float (JsArray [JsString "Foo"])
            |> shouldEqualResult (Error (Single ("root" , "Expected a numeric value")))

        [<Fact>]
        let ``float should not decode an object`` () =
            decode Decoder.float (JsObject (Map [("Foo", JsString "Foo")]))
            |> shouldEqualResult (Error (Single ("root" , "Expected a numeric value")))

        [<Fact>]
        let ``float should not decode null`` () =
            decode Decoder.float JsNull
            |> shouldEqualResult (Error (Single ("root" , "Expected a numeric value")))

        // array

        [<Fact>]
        let ``array should not decode an integer`` () =
            decode (Decoder.array (Decoder.succeed ())) (JsInteger 1)
            |> shouldEqualResult (Error (Single ("root" , "Expected an array")))

        [<Fact>]
        let ``array should not decode a string`` () =
            decode (Decoder.array (Decoder.succeed ())) (JsString "Foo")
            |> shouldEqualResult (Error (Single ("root" , "Expected an array")))

        [<Fact>]
        let ``array should not decode a boolean`` () =
            decode (Decoder.array (Decoder.succeed ())) (JsBoolean false)
            |> shouldEqualResult (Error (Single ("root" , "Expected an array")))

        [<Fact>]
        let ``array should not decode an float`` () =
            decode (Decoder.array (Decoder.succeed ())) (JsFloat 1.0)
            |> shouldEqualResult (Error (Single ("root" , "Expected an array")))

        [<Fact>]
        let ``array should not decode an object`` () =
            decode (Decoder.array (Decoder.succeed ())) (JsObject (Map [("Foo", JsString "Foo")]))
            |> shouldEqualResult (Error (Single ("root" , "Expected an array")))

        [<Fact>]
        let ``array should not decode null`` () =
            decode (Decoder.array (Decoder.succeed ())) JsNull
            |> shouldEqualResult (Error (Single ("root" , "Expected an array")))

        [<Fact>]
        let ``array should decode an array`` () =
            decode (Decoder.array Decoder.int) (JsArray [JsInteger 1; JsInteger 2])
            |> shouldEqualResult (Ok [1; 2])

        [<Fact>]
        let ``array should aggregate failures`` () =
            decode (Decoder.array Decoder.string) (JsArray [JsInteger 1; JsInteger 2])
            |> shouldEqualResult (Error (Aggregate [Single ("root[0]", "Expected a string value"); Single ("root[1]", "Expected a string value")]))

        // nullable

        [<Fact>]
        let ``nullable should decode null`` () =
            decode (Decoder.nullable Decoder.string) JsNull |> shouldEqualResult (Ok None)

        [<Fact>]
        let ``nullable should decode its type`` () =
            decode (Decoder.nullable Decoder.string) (JsString "Foo") |> shouldEqualResult (Ok (Some "Foo"))

        [<Fact>]
        let ``nullable should fail when its decoder does`` () =
            decode (Decoder.nullable Decoder.string) (JsInteger 1)
            |> shouldEqualResult (Error (Single ("root" , "Expected a string value")))
