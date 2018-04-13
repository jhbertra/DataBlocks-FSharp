namespace DataBlocks

open Aether
open Aether.Operators
open FSharpPlus

// Prelude

type Void = private | Void

module Prelude =
    
    let absurd = function
    | Void -> raise (System.InvalidOperationException("This function is logically impossible to call"))

open Prelude

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

    let issueId : Lens<Issue, string> = Issue.Id

    let issueMessage : Lens<Issue, string> = Issue.Message

    let decoderId<'a, 'b> : Lens<Decoder<'a, 'b>, string> =
        Decoder<'a, 'b>.Id ()

    let decoderDecode<'a, 'b> : Lens<Decoder<'a, 'b>, (string -> 'a -> Result<'b, DecodeError>)> =
        Decoder<'a, 'b>.Decode ()

    let encoderEncode<'a, 'b> : Lens<Encoder<'a, 'b>, ('a -> 'b)> =
         (fun (Encoder e) -> e) , (fun e _ -> Encoder e) 

    let unfixedBlockDecoder<'a, 'b, 'c> : Lens<DataBlock<'a, 'b, 'c>, Decoder<'a, 'b>> =
        DataBlock<'a, 'b, 'c>.Decoder()

    let unfixedBlockEncoder<'a, 'b, 'c> : Lens<DataBlock<'a, 'b, 'c>, Encoder<'c, 'a>> =
        DataBlock<'a, 'b, 'c>.Encoder()

    let unfixedBlockId<'a, 'b, 'c> = unfixedBlockDecoder<'a, 'b, 'c> >-> decoderId

    let unfixedBlockDecode<'a, 'b, 'c> = unfixedBlockDecoder<'a, 'b, 'c> >-> decoderDecode

    let unfixedBlockEncode<'a, 'b, 'c> = unfixedBlockEncoder<'a, 'b, 'c> >-> encoderEncode

    let blockUnfixed<'a, 'b> : Lens<DataBlock<'a, 'b>, DataBlock<'a, 'b, 'b>> =
        DataBlock<'a, 'b>.Unfixed()

    let blockDecoder<'a, 'b> = blockUnfixed<'a, 'b> >-> unfixedBlockDecoder

    let blockEncoder<'a, 'b> = blockUnfixed<'a, 'b> >-> unfixedBlockEncoder

    let blockId<'a, 'b> = blockUnfixed<'a, 'b> >-> unfixedBlockId

    let blockDecode<'a, 'b> = blockUnfixed<'a, 'b> >-> unfixedBlockDecode

    let blockEncode<'a, 'b> = blockUnfixed<'a, 'b> >-> unfixedBlockEncode

    let runDecoder d = d.decode d.id

    let runEncoder (Encoder e) = e

    let decode (Fixed b) = runDecoder b.decoder

    let encode (Fixed b) = runEncoder b.encoder


    module Decoder =


        let fromResult r = decoder "" (fun id _ -> Result.mapError (fun msg -> { id = id; message = msg } |> Single) r)


        let succeed a = Ok a |> fromResult


        let fail message = Error message |> fromResult


        // instance Functor where
        let map f d = decoder d.id (fun id -> d.decode id >=> (Ok << f))

        // instance Applicative where
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

        // instance Monad where
        let bind f d =
            decoder
                d.id
                (fun id a -> 
                    monad {
                        let! b = d.decode id a
                        return! (f b).decode id a
                    }
                )

        // instance Alternative where
        let alternate d1 d2 =
            decoder
                d1.id
                (fun a id ->
                    match d1.decode a id with
                    | Error _ -> d2.decode a id
                    | x -> x
                )

        // instance Contravariant where
        let contramap f d = decoder d.id (fun id -> f >> d.decode id)

        // instance Profunctor where
        let dimap f g d = decoder d.id (fun id -> f >> d.decode id >=> (Ok << g))

        // instance Category where
        let compose f g = decoder f.id (fun id -> f.decode id >=> g.decode id)

        // instance Arrow where
        let fromFunction f = decoder "" (fun _ -> Ok << f)

        let first f = decoder f.id (fun id (a, c) -> f.decode id a |> (c |> flip tuple2 |> Result.map))


    module Encoder =

        // instance Functor where
        let map f (Encoder e) = Encoder (f << e)

        // instance Contravariant where
        let contramap f (Encoder e) = Encoder (e << f)

        // instance Divisible where
        let divide (combine : 'd -> 'd -> 'd) splitA (Encoder e1) (Encoder e2) =
            Encoder (splitA >> ( e1 *** e2 ) >> (fun (d, d') -> combine d d'))

        let divided combine = divide combine id
            
        let conquer zero = Encoder <| konst zero

        // instance Decidable where
        let lose f = Encoder (absurd << f)

        let lost () = lose id

        let choose splitA (Encoder e1) (Encoder e2) =
            Encoder (splitA >> fanin e2 e1)

        let chosen () = choose id        

        // instance Profunctor where
        let dimap f g (Encoder e) = Encoder (g << e << f)

        // instance Category where
        let idEncoder = Encoder id

        let compose (Encoder f) (Encoder g) = Encoder (f >> g)

        // instance Arrow where
        let first (Encoder e) = Encoder (fun (a, c) -> e a |> (flip tuple2) c)


    // Applicative + Divisible = ...Clutch?
    let disengage zero f = { decoder = Decoder.succeed f; encoder = Encoder.conquer zero }

    let part combine getter (Fixed partBlock) remainderBlock =
        { decoder = Decoder.apply remainderBlock.decoder partBlock.decoder
          encoder = Encoder.divide combine (fanout getter id) partBlock.encoder remainderBlock.encoder
        }

    let engage = Fixed

    // Alternative + Decidable = Tracable? :/
    let path getter wrapper (Fixed pathBlock) (Fixed remainderBlock) =
        Fixed
            { decoder = Decoder.alternate (Decoder.map wrapper pathBlock.decoder) remainderBlock.decoder
              encoder =
                Encoder.choose
                    (fun a ->  getter a |> Option.map Choice2Of2 |> Option.defaultValue (Choice1Of2 a))
                    remainderBlock.encoder
                    pathBlock.encoder
            }

    let exhausted zero = Fixed { decoder = Decoder.fail "Alternative set void"; encoder = Encoder (konst zero) }

    let invmap2 f g h i (Fixed b) =
        Fixed
            { decoder = Decoder.dimap g h b.decoder
              encoder = Encoder.dimap i f b.encoder
            }

    let invmapRaw f g = invmap2 f g id id

    let invmapDecoded f g = invmap2 id id f g

    let epimap2 f g h i (Fixed b) =
        let wrapError id = Result.mapError (fun msg -> { id = id; message = msg } |> Single)
        Fixed
            { decoder = decoder b.decoder.id (fun id -> ((g >> wrapError id) >=> b.decoder.decode id >=> (h >> wrapError id)))
              encoder = Encoder.dimap i f b.encoder
            }

    let epimapRaw f g = epimap2 f g (Ok << id) id

    let epimapDecoded f g = epimap2 id (Ok << id) f g


    let modify block f = decode block >> map (f >> encode block)


    let operate block f = decode block >=> f >=> (encode block >> Ok)


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


type DataBlock<'a, 'b> with

    static member RightInvmap(f : 'b -> 'c, g : 'c -> 'b, (Fixed b) : DataBlock<'a, 'b>) : DataBlock<'a, 'c> =
        Fixed
            { decoder = map f b.decoder
              encoder = contramap g b.encoder
            }

    static member LeftInvmap(f : 'c -> 'a, g : 'a -> 'c, (Fixed b) : DataBlock<'a, 'b>) : DataBlock<'c, 'b> =
        Fixed
            { decoder = contramap f b.decoder
              encoder = map g b.encoder
            }

    static member Invmap2(f : 'a -> 'c, g : 'c -> 'a, h : 'b -> 'd, i : 'd -> 'b, b : DataBlock<'a, 'b>) : DataBlock<'c, 'd> =
        invmap2 f g h i b
    
    static member get_Id() : DataBlock<'a, 'a> =
        Fixed
            { decoder = catId
              encoder = catId
            }

    static member (<<<) ((Fixed f) : DataBlock<'b, 'c>, (Fixed g) : DataBlock<'a, 'b>) : DataBlock<'a, 'c> =
        Fixed
            { decoder = Decoder.compose g.decoder f.decoder
              encoder = Encoder.compose f.encoder g.encoder
            }

    static member (<->) (f : 'a -> 'b, g : 'b -> 'a) : DataBlock<'a, 'b> =
        Fixed
            { decoder = Decoder.fromFunction f
              encoder = Encoder g
            }