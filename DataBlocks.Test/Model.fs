namespace DataBlocks.Test

module Model =

    type PositiveInteger =
        PositiveInteger of int
        with

            static member Create i =
                if i > 0 then
                    Ok (PositiveInteger i)
                else
                    Error "Value must be positive"

            static member Unpack (PositiveInteger i) = i
                        


    type ChildFieldType =
        | Choice1 of int
        | Choice2 of string        


    type SuperAdvancedOptions = {
        childField: ChildFieldType
    }


    let superAdvancedOptions childField = { childField = childField }
    let childField { childField = childField } = childField


    type AdvancedOptions = {
        allowNonsecure : bool
        superAdvanced : SuperAdvancedOptions
    }


    let advancedOptions allowNonsecure superAdvanced = {
        allowNonsecure = allowNonsecure
        superAdvanced = superAdvanced
    }
    let allowNonsecure { allowNonsecure = allowNonsecure } = allowNonsecure
    let superAdvanced { superAdvanced = superAdvanced } = superAdvanced


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
    let url { url = url } = url
    let connectionLimit { connectionLimit = connectionLimit } = connectionLimit
    let trustedUrls { trustedUrls = trustedUrls } = trustedUrls
    let email { email = email } = email
    let advanced { advanced = advanced } = advanced