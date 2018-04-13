namespace DataBlocks.Test


open Xunit
open FsUnit.Xunit

open FSharpPlus.Operators

open DataBlocks
open DataBlocks.Core
open DataBlocks.Json

module JsonBlockTests =

    let private isOk = function
    | Ok a -> a
    | Error x -> raise(new MatchException("Ok", sprintf "%A" x, null))

    let private isError = function
    | Ok a -> raise(new MatchException("Error", sprintf "%A" a, null))
    | Error x -> x


    [<Fact>]
    let ``string should encode a string`` () =
        encode string "Foo" |> should equal (JsString "Foo")


    [<Fact>]
    let ``string should decode a JsString`` () =
        decode string (JsString "Foo") |> isOk |> should equal "Foo"


    [<Fact>]
    let ``decodeString should decode a raw string`` () =
        decodeString string "\"Foo\"" |> isOk |> should equal "Foo"
        

    [<Fact>]
    let ``int should encode an integer`` () =
        encode int 1 |> should equal (JsInteger 1)


    [<Fact>]
    let ``int should decode a JsInteger`` () =
        decode int (JsInteger 1) |> isOk |> should equal 1   


    [<Fact>]
    let ``decodeString should decode a raw int`` () =
        decodeString int "1" |> isOk |> should equal 1   
        

    [<Fact>]
    let ``bool should encode a bool`` () =
        encode bool true |> should equal (JsBoolean true)


    [<Fact>]
    let ``bool should decode a JsBool`` () =
        decode bool (JsBoolean true) |> isOk |> should equal true


    [<Fact>]
    let ``decodeString should decode a raw bool`` () =
        decodeString bool "true" |> isOk |> should equal true
        

    [<Fact>]
    let ``float should encode a float`` () =
        encode float 2.5 |> should equal (JsFloat 2.5)


    [<Fact>]
    let ``float should decode a JsFloat`` () =
        decode float (JsFloat 2.5) |> isOk |> should equal 2.5


    [<Fact>]
    let ``decodeString should decode a raw float`` () =
        decodeString float "2.5" |> isOk |> should equal 2.5
        

    [<Fact>]
    let ``nullable should encode None`` () =
        encode (nullable string) None |> should equal JsNull
        

    [<Fact>]
    let ``nullable should encode Some`` () =
        encode (nullable string) (Some "Foo") |> should equal (JsString "Foo")
        

    [<Fact>]
    let ``nullable should decode None`` () =
        decode (nullable string) JsNull |> isOk |> should equal None
        
        

    [<Fact>]
    let ``nullable should decode Some`` () =
        decode (nullable string) (JsString "Foo") |> isOk |> should equal (Some "Foo")
        

    [<Fact>]
    let ``array should encode an array`` () =
        encode (array int) [0; 1; 2] 
        |> should equal (JsArray [JsInteger 0; JsInteger 1; JsInteger 2])
        

    [<Fact>]
    let ``array should decode a JsArray`` () =
        decode (array int) (JsArray [JsInteger 0; JsInteger 1; JsInteger 2]) 

    [<Fact>]
    let ``decodeString should decode a raw array`` () =
        decodeString (array int) "[0, 1, 2]"
        |> isOk
        |> should equal [0; 1; 2]
        

    [<Fact>]
    let ``object chaining should encode the object`` () =
        (1, Some true, "test")
        |> encode
          ( tuple3
            |> ``{``
            |> required "foo" item1 int
            |> optional "bar" item2 bool
            |> required "qux" item3 string
            |> ``}``
          )
        |> should
            equal
            ( JsObject
                ( Map
                    [ ( "foo" , JsInteger 1 )
                      ( "bar" , JsBoolean true )
                      ( "qux" , JsString "test" )
                    ]
                )
            )


    [<Fact>]
    let ``object chaining should decode the object`` () =
        JsObject
            ( Map
                [ ( "foo" , JsInteger 1 )
                  ( "bar" , JsBoolean true )
                  ( "qux" , JsString "test" )
                ]
            )
        |> decode
          ( tuple3
            |> ``{``
            |> required "foo" item1 int
            |> optional "bar" item2 bool
            |> required "qux" item3 string
            |> ``}``
          )
        |> isOk
        |> should equal (1, Some true, "test")


    [<Fact>]
    let ``decodeString should decode a raw object`` () =
        """
        {
            "foo": 1,
            "bar": true,
            "qux": "test"
        }
        """        
        |> decodeString
          ( tuple3
            |> ``{``
            |> required "foo" item1 int
            |> optional "bar" item2 bool
            |> required "qux" item3 string
            |> ``}``
          )
        |> isOk
        |> should equal (1, Some true, "test")
        

    // Quasi-realistic scenario

    open DataBlocks.Test.Model


    let childFieldTypeBlock =
        switch
        |> choice (function Choice1 x -> Some x | _ -> None) Choice1 int
        |> choice (function Choice2 x -> Some x | _ -> None) Choice2 string


    let superAdvancedOptionsBlock =
        superAdvancedOptions
        |> ``{``
        |> required "childField" childField childFieldTypeBlock
        |> ``}``


    let advancedOptionsBlock =
        advancedOptions
        |> ``{``
        |> required "allowNonsecure" allowNonsecure bool
        |> required "superAdvanced" superAdvanced superAdvancedOptionsBlock
        |> ``}``


    let imaginationConfigBlock =
        imaginationConfig
        |> ``{``
        |> required "url" url string
        |> required "connectionLimit" connectionLimit (epimapDecoded PositiveInteger.Create PositiveInteger.Unpack int)
        |> required "trustedUrls" trustedUrls (array string)
        |> optional "email" email string
        |> required "advanced" advanced advancedOptionsBlock
        |> ``}``


    [<Fact>]
    let ``The API should suitably encode a more than trivial example`` () =
        encode
            imaginationConfigBlock
            { url = "http://www.google.com"
              connectionLimit = PositiveInteger 1
              trustedUrls = ["url1"]
              email = Some "asdf@test.com"
              advanced =
                { allowNonsecure = false
                  superAdvanced = { childField = Choice1 1 }
                }
            }
        |> should
            equal
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


    [<Fact>]
    let ``The API should suitably decode a more than trivial example`` () =
        decode
            imaginationConfigBlock
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


    [<Fact>]
    let ``decodeString should suitably decode a more than trivial example`` () =
        decodeString
            imaginationConfigBlock
            """
            {
                "url": "http://www.google.com",
                "connectionLimit": 1,
                "trustedUrls": ["url1"],
                "email": "asdf@test.com",
                "advanced": {
                    "allowNonsecure": false,
                    "superAdvanced": { "childField": 1 }
                }
            }
            """
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