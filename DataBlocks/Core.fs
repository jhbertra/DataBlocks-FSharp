namespace DataBlocks

open Aether
open Aether.Operators
open FSharpPlus

// Types

type Issue =
  { id: string
    message: string }

    static member Id =
        (fun issue -> issue.id) , (fun id issue -> { issue with id = id })

    static member Message =
        (fun issue -> issue.message) , (fun message issue -> { issue with message = message })


type DecodeError =
    | Single of Issue
    | Multiple of Issue list with

    static member (+) ( e1 , e2 ) =
        match ( e1 , e2 ) with
        | ( Single e1 , Single e2) -> Multiple [e1; e2]
        | ( Single e , Multiple es ) -> Multiple (e :: es)
        | ( Multiple es , Single e ) -> Multiple (es @ [e])
        | ( Multiple es1 , Multiple es2 ) -> Multiple (es1 @ es2)

    static member get_Zero() = Multiple []


type Decoder<'a, 'b> =
  { decode : string -> 'a -> Result<'b, DecodeError>
    id : string }

    static member Id () =
        (fun decoder -> decoder.id) , (fun id decoder  -> { decoder with id = id; decode = decoder.decode })

    static member Decode () =
        (fun decoder -> decoder.decode) , (fun d decoder -> { decoder with decode = d })


type Encoder<'a, 'b> = Encoder of ('a -> 'b)


type DataBlock<'a, 'b, 'c> =
  private { decoder : Decoder<'a, 'b>
            encoder : Encoder<'c, 'a> }

    static member Decoder () =
        (fun block -> block.decoder) , (fun decoder block  -> { block with decoder = decoder })

    static member Encoder () =
        (fun block -> block.encoder) , (fun encoder block  -> { block with encoder = encoder })


type DataBlock<'a, 'b> = Fixed of DataBlock<'a, 'b, 'b> with

    static member Unfixed () =
        (fun (Fixed block) -> block) , (fun newBlock (Fixed _)  -> Fixed newBlock)


module Core =

    // Constructors

    let issue id message = { id = id; message = message }


    let error id message = Single (issue id message )


    let decoder id decode = { id = id; decode = decode }


    let encoder = Encoder


    let dataBlock id decode encode = Fixed { decoder = decoder id decode; encoder = encoder encode }



    // Optics

    let issueId = Lens Issue.Id

    let issueMessage = Lens Issue.Message

    let decoderId<'a, 'b> : Lens<Decoder<'a, 'b>, string> =
        Lens (Decoder<'a, 'b>.Id ())

    let decoderDecode<'a, 'b> : Lens<Decoder<'a, 'b>, (string -> 'a -> Result<'b, DecodeError>)> =
        Lens (Decoder<'a, 'b>.Decode ())

    let encoderEncode<'a, 'b> : Lens<Encoder<'a, 'b>, ('a -> 'b)> =
        Lens ( (fun (Encoder e) -> e) , (fun e _ -> Encoder e) )

    let unfixedBlockDecoder<'a, 'b, 'c> : Lens<DataBlock<'a, 'b, 'c>, Decoder<'a, 'b>> =
        Lens (DataBlock<'a, 'b, 'c>.Decoder())

    let unfixedBlockEncoder<'a, 'b, 'c> : Lens<DataBlock<'a, 'b, 'c>, Encoder<'c, 'a>> =
        Lens (DataBlock<'a, 'b, 'c>.Encoder())

    let unfixedBlockId<'a, 'b, 'c> = unfixedBlockDecoder<'a, 'b, 'c> >-> decoderId

    let unfixedBlockDecode<'a, 'b, 'c> = unfixedBlockDecoder<'a, 'b, 'c> >-> decoderDecode

    let unfixedBlockEncode<'a, 'b, 'c> = unfixedBlockEncoder<'a, 'b, 'c> >-> encoderEncode

    let blockUnfixed<'a, 'b> : Lens<DataBlock<'a, 'b>, DataBlock<'a, 'b, 'b>> =
        Lens (DataBlock<'a, 'b>.Unfixed())

    let blockDecoder<'a, 'b> = blockUnfixed<'a, 'b> >-> unfixedBlockDecoder

    let blockEncoder<'a, 'b> = blockUnfixed<'a, 'b> >-> unfixedBlockEncoder

    let blockId<'a, 'b> = blockUnfixed<'a, 'b> >-> unfixedBlockId

    let blockDecode<'a, 'b> = blockUnfixed<'a, 'b> >-> unfixedBlockDecode

    let blockEncode<'a, 'b> = blockUnfixed<'a, 'b> >-> unfixedBlockEncode


    module Decoder =


        let fromResult r = decoder "" (fun _ _ -> r)


        let succeed a = Ok a |> fromResult


        let fail e = Error e |> fromResult


        // Instance Functor where
        let map f d = decoder d.id (fun id -> d.decode id >=> (Ok << f))

        // Instance Applicative where
        let apply dfa da = 
            decoder
                dfa.id
                (fun a id ->
                    let result1 = dfa.decode a id
                    let result2 = da.decode a id
                    match ( result1 , result2 ) with
                    | ( Error e1 , Error e2 ) -> Error (e1 ++ e2)
                    | ( _ , Error e2 ) -> Error e2
                    | ( Error e1 , _ ) -> Error e1
                    | ( Ok fa, Ok a ) -> Ok (fa a)
                )


        // Instance Monad where
        let bind f d =
            decoder
                d.id
                (fun id a -> 
                    monad {
                        let! b = d.decode id a
                        return! (f b).decode id a
                    }
                )


        // Instance Alternative where
        let alternate d1 d2 =
            decoder
                d1.id
                (fun a id ->
                    match d1.decode a id with
                    | Error _ -> d2.decode a id
                    | x -> x
                )


        // Instance Contravariant where
        let contramap f d = decoder d.id (fun id -> f >> d.decode id)


        // Instance Profunctor where
        let dimap f g d = decoder d.id (fun id -> f >> d.decode id >=> (Ok << g))


        // Instance Category where
        let compose f g = decoder f.id (fun id -> f.decode id >=> g.decode id)


        // Instance Arrow where
        let fromFunction f = decoder "" (fun _ -> Ok << f)


        let first f = decoder f.id (fun id (a, c) -> f.decode id a |> (c |> flip tuple2 |> Result.map))


    module Encoder =

        // Instance Functor where
        let map f (Encoder e) = Encoder (f << e)


        // Instance Contravariant where
        let contramap f (Encoder e) = Encoder (e << f)


        // Instance Profunctor where
        let dimap f g (Encoder e) = Encoder (g << e << f)


        // Instance Category where
        let idEncoder = Encoder id


        let compose (Encoder f) (Encoder g) = Encoder (f >> g)


        // Instance Arrow where
        let first (Encoder e) = Encoder (fun (a, c) -> e a |> (flip tuple2) c)


open Core

type Decoder<'a, 'b> with

    static member Return (b : 'b) : Decoder<'a, 'b> = Decoder.succeed b

    static member Map ( d : Decoder<'a, 'b> , f : 'b -> 'c ) : Decoder<'a, 'c> =
        Decoder.map f d

    static member (<*>) ( dfb : Decoder<'a, 'b -> 'c> , db : Decoder<'a, 'b> ) : Decoder<'a, 'c> =
        Decoder.apply dfb db

    static member (>>=) ( d : Decoder<'a, 'b> , f : 'b -> Decoder<'a, 'c> ) : Decoder<'a, 'c> =
        Decoder.bind f d

    static member (<|>) ( d1 : Decoder<'a, 'b> , d2 : Decoder<'a, 'b> ) : Decoder<'a, 'b> =
        Decoder.alternate d1 d2

    static member Contramap ( d : Decoder<'a, 'b> , f : 'c -> 'a ) : Decoder<'c, 'b> =
        Decoder.contramap f d

    static member Dimap ( d : Decoder<'a, 'b> , f : 'c -> 'a , g : 'b -> 'd  ) : Decoder<'c, 'd> =
        Decoder.dimap f g d

    static member get_Id () : Decoder<'a, 'a> = decoder "" (fun _ -> (Ok << id))

    static member (<<<) (f : Decoder<'b, 'c>, g : Decoder<'a, 'b>) : Decoder<'a, 'c> =
        Decoder.compose g f

    static member Arr(f : 'a -> 'b) : Decoder<'a, 'b> =
        Decoder.fromFunction f

    static member First(f : Decoder<'a, 'b>) : Decoder<'a * 'c, 'b * 'c> =
        Decoder.first f


type Encoder<'a, 'b> with

    static member Return : 'b -> Encoder<'a, 'b> = Encoder << konst

    static member Map ( d : Encoder<'a, 'b> , f : 'b -> 'c ) : Encoder<'a, 'c> =
        Encoder.map f d

    static member Contramap ( d : Encoder<'a, 'b> , f : 'c -> 'a ) : Encoder<'c, 'b> =
        Encoder.contramap f d

    static member Dimap ( d : Encoder<'a, 'b> , f : 'c -> 'a , g : 'b -> 'd  ) : Encoder<'c, 'd> =
        Encoder.dimap f g d

    static member get_Id() : Encoder<'a, 'a> = Encoder.idEncoder

    static member (<<<) (f : Encoder<'b, 'c>, g : Encoder<'a, 'b>) : Encoder<'a, 'c> =
        Encoder.compose g f

    static member Arr(f : 'a -> 'b) : Encoder<'a, 'b> = Encoder f

    static member First(f : Encoder<'a, 'b>) : Encoder<'a * 'c, 'b * 'c> =
        Encoder.first f