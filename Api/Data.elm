module Api.Data exposing
    ( Data(..)
    , DataError(..)
    , expectInt
    , expectJson
    , expectString
    , expectWhatever
    , isSuccess
    , map
    , toMaybe, stringForGenericErrors
    )

-- import Constants

import Dict
import Http
import Json.Decode as Json


type Data value
    = NotAsked
    | Loading
    | Failure (List DataError)
    | Success value


type DataError
    = GenericError String
    | FieldError String String


toMaybe : Data value -> Maybe value
toMaybe data =
    case data of
        Success value ->
            Just value

        _ ->
            Nothing


map : (value -> otherValue) -> Data value -> Data otherValue
map mapper data =
    case data of
        Success value ->
            Success (mapper value)

        Failure failure ->
            Failure failure

        Loading ->
            Loading

        NotAsked ->
            NotAsked


expectWhatever : msg -> Http.Expect msg
expectWhatever toMsg =
    Http.expectStringResponse (fromResult >> (\_ -> toMsg)) <|
        \response ->
            case response of
                Http.BadUrl_ _ ->
                    Err [ GenericError "Bad URL" ]

                Http.Timeout_ ->
                    Err [ GenericError "Request timeout" ]

                Http.NetworkError_ ->
                    Err [ GenericError "Connection issues" ]

                Http.BadStatus_ metadata body ->
                    decodeBadStatus metadata body

                Http.GoodStatus_ _ body ->
                    Ok ()


expectString : (Data String -> msg) -> Http.Expect msg
expectString toMsg =
    Http.expectStringResponse (fromResult >> toMsg) <|
        \response ->
            case response of
                Http.BadUrl_ _ ->
                    Err [ GenericError "Bad URL" ]

                Http.Timeout_ ->
                    Err [ GenericError "Request timeout" ]

                Http.NetworkError_ ->
                    Err [ GenericError "Connection issues" ]

                Http.BadStatus_ metadata body ->
                    decodeBadStatus metadata body

                Http.GoodStatus_ _ body ->
                    Ok body


expectInt : (Data Int -> msg) -> Http.Expect msg
expectInt toMsg =
    Http.expectStringResponse (fromResult >> toMsg) <|
        \response ->
            case response of
                Http.BadUrl_ _ ->
                    Err [ GenericError "Bad URL" ]

                Http.Timeout_ ->
                    Err [ GenericError "Request timeout" ]

                Http.NetworkError_ ->
                    Err [ GenericError "Connection issues" ]

                Http.BadStatus_ metadata body ->
                    decodeBadStatus metadata body

                Http.GoodStatus_ _ body ->
                    case String.toInt body of
                        Just int ->
                            Ok int

                        Nothing ->
                            Err [ GenericError "Bad int" ]


expectJson : (Data value -> msg) -> Json.Decoder value -> Http.Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse (fromResult >> toMsg) <|
        \response ->
            case response of
                Http.BadUrl_ _ ->
                    Err [ GenericError "Bad URL" ]

                Http.Timeout_ ->
                    Err [ GenericError "Request timeout" ]

                Http.NetworkError_ ->
                    Err [ GenericError "Connection issues" ]

                Http.BadStatus_ metadata body ->
                    case Json.decodeString errorDecoder body of
                        Ok errors ->
                            Err errors

                        Err _ ->
                            decodeBadStatus metadata body

                Http.GoodStatus_ _ body ->
                    case Json.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            -- if String.length Constants.env == 0 then
                            --     -- Dev
                            --     Err [ GenericError (Json.errorToString err) ]
                            -- else
                            Err [ GenericError "Something went wrong" ]


errorDecoder : Json.Decoder (List DataError)
errorDecoder =
    Json.keyValuePairs (Json.list Json.string)
        |> Json.field "errors"
        |> Json.map (List.concatMap (\( key, values ) -> values |> List.map (\value -> FieldError key value)))


fromResult : Result (List DataError) value -> Data value
fromResult result =
    case result of
        Ok value ->
            Success value

        Err reasons ->
            Failure reasons


isSuccess : Data value -> Bool
isSuccess data =
    case data of
        Success _ ->
            True

        _ ->
            False


decodeBadStatus metadata body =
    -- if String.length Constants.env == 0 then
    --     -- Dev
    Err
        [ GenericError
            ("Bad status code: "
                ++ String.fromInt metadata.statusCode
                ++ "\n"
                ++ (metadata.headers
                        |> Dict.toList
                        |> List.map (\( k, v ) -> "\t" ++ k ++ " : " ++ v)
                        |> String.join "\n"
                   )
                ++ "\n"
                ++ metadata.statusText
                ++ "\n"
                ++ body
            )
        ]


stringForGenericErrors : List DataError -> String
stringForGenericErrors errors =
    errors
        |> List.filterMap
            (\errorType ->
                case errorType of
                    GenericError error ->
                        Just error

                    _ ->
                        Nothing
            )
        |> String.join "\n"
