namespace DataBlocks.Test


open Xunit
open FsUnit.Xunit

open FSharpPlus.Operators

open DataBlocks
open DataBlocks.Json

module JsonEncoderTests =

    [<Fact>]
    let ``string should encode a string`` () =
        runEncoder Encoder.string "Foo" |> should equal (JsString "Foo")
        
        

    [<Fact>]
    let ``int should encode an integer`` () =
        runEncoder Encoder.int 1 |> should equal (JsInteger 1)
        
        

    [<Fact>]
    let ``bool should encode a bool`` () =
        runEncoder Encoder.bool true |> should equal (JsBoolean true)
        
        

    [<Fact>]
    let ``float should encode a float`` () =
        runEncoder Encoder.float 2.5 |> should equal (JsFloat 2.5)
        
        

    [<Fact>]
    let ``nullable should encode None`` () =
        runEncoder (Encoder.nullable Encoder.string) None |> should equal JsNull
        
        

    [<Fact>]
    let ``array should encode an array`` () =
        runEncoder (Encoder.array Encoder.int) [0; 1; 2] 
        |> should equal (JsArray [JsInteger 0; JsInteger 1; JsInteger 2])

        

    [<Fact>]
    let ``object chaining should decode the object`` () =
        (1, true, "test")
        |> runEncoder
          ( Encoder.object
            |> Encoder.property "foo" item1 Encoder.int
            |> Encoder.property "bar" item2 Encoder.bool
            |> Encoder.property "qux" item3 Encoder.string
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
        

    // Quasi-realistic scenario

    open DataBlocks.Test.Model
    open Encoder


    let childFieldTypeEncoder =
        switch
        |> choice (function Choice1 x -> Some x | _ -> None) int
        |> choice (function Choice2 x -> Some x | _ -> None) string


    let superAdvancedOptionsEncoder =
        object
        |> property "childField" childField childFieldTypeEncoder


    let advancedOptionsEncoder =
        object
        |> property "allowNonsecure" allowNonsecure bool
        |> property "superAdvanced" superAdvanced superAdvancedOptionsEncoder


    let imaginationConfigEncoder =
        object
        |> property "url" url string
        |> property "connectionLimit" connectionLimit (contramap PositiveInteger.Unpack int)
        |> property "trustedUrls" trustedUrls (array string)
        |> property "email" email (nullable string)
        |> property "advanced" advanced advancedOptionsEncoder


    [<Fact>]
    let ``The API should suitably handle a more than trivial examples`` () =
        runEncoder
            imaginationConfigEncoder
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