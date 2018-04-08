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
  { decode : 'a -> Result<'b, DecodeError> 
    id : string }

    static member Id () =
        (fun decoder -> decoder.id) , (fun id decoder  -> { decoder with id = id; decode = decoder.decode })

    static member Decode () =
        (fun decoder -> decoder.decode) , (fun d decoder -> { decoder with decode = d })


type Encoder<'a, 'b> = Encoder of ('b -> 'a)


type DataBlock<'a, 'b> =
  private { decoder : Decoder<'a, 'b> 
            encoder : Encoder<'a, 'b> }

    static member Decoder () =
        (fun block -> block.decoder) , (fun decoder block  -> { block with decoder = decoder })

    static member Encoder () =
        (fun block -> block.encoder) , (fun encoder block  -> { block with encoder = encoder })


type DataBlock<'a> = Fixed of DataBlock<'a, 'a> with

    static member Unfixed () =
        (fun (Fixed block) -> block) , (fun newBlock (Fixed _)  -> Fixed newBlock)


module Core =

    // Optics

    let issueId = Lens Issue.Id

    let issueMessage = Lens Issue.Message

    let decoderId<'a, 'b> : Lens<Decoder<'a, 'b>, string> =
        Lens (Decoder<'a, 'b>.Id ())

    let decoderDecode<'a, 'b> : Lens<Decoder<'a, 'b>, ('a -> Result<'b, DecodeError>)> =
        Lens (Decoder<'a, 'b>.Decode ())

    let encoderEncode<'a, 'b> : Lens<Encoder<'a, 'b>, ('b -> 'a)> =
        Lens ( (fun (Encoder e) -> e) , (fun e _ -> Encoder e) )

    let unfixedBlockDecoder<'a, 'b> : Lens<DataBlock<'a, 'b>, Decoder<'a, 'b>> =
        Lens (DataBlock<'a, 'b>.Decoder())

    let unfixedBlockEncoder<'a, 'b> : Lens<DataBlock<'a, 'b>, Encoder<'a, 'b>> =
        Lens (DataBlock<'a, 'b>.Encoder())

    let unfixedBlockId<'a, 'b> = unfixedBlockDecoder<'a, 'b> >-> decoderId

    let unfixedBlockDecode<'a, 'b> = unfixedBlockDecoder<'a, 'b> >-> decoderDecode

    let unfixedBlockEncode<'a, 'b> = unfixedBlockEncoder<'a, 'b> >-> encoderEncode

    let blockUnfixed<'a> : Lens<DataBlock<'a>, DataBlock<'a, 'a>> =
        Lens (DataBlock<'a>.Unfixed())

    let blockDecoder<'a> = blockUnfixed<'a> >-> unfixedBlockDecoder

    let blockEncoder<'a> = blockUnfixed<'a> >-> unfixedBlockEncoder

    let blockId<'a> = blockUnfixed<'a> >-> unfixedBlockId

    let blockDecode<'a> = blockUnfixed<'a> >-> unfixedBlockDecode

    let blockEncode<'a> = blockUnfixed<'a> >-> unfixedBlockEncode