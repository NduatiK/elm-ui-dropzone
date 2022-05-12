module Extra.DropZone exposing
    ( init, Model
    , update, Msg(..)
    , subscriptions
    , viewProgresses, viewDropzone
    , InnerMsg, viewCustomDropzone
    )

{-|

@docs init, Model
@docs update, Msg
@docs subscriptions
@docs viewProgresses, viewDropzone

-}

-- import Api.Errors

import Api.Data as Data exposing (Data)
import Colors
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Extra.Mime as Mime
import FeatherIcons
import File exposing (File)
import File.Select
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Process
import Shared
import Svg
import Svg.Attributes
import Task



-- import UI


type Model auth image
    = Model
        { highlightDropzone : Bool
        , files : List FileUpload
        , uid : Int
        , uploadImageEndpoint : UploadMethod auth image
        , bgColor : Color
        , color : Color
        }


type UploadMethod auth image
    = HttpUpload
        (auth
         ->
            { file : File.File
            , tracker : String
            , onResponse : Data image -> InnerMsg image
            }
         -> Cmd (InnerMsg image)
        )
    | PortUpload (String -> File.File -> Cmd (InnerMsg image))


init : { uploadImageEndpoint : UploadMethod auth image, bgColor : Color, color : Color } -> Model auth image
init opts =
    Model
        { highlightDropzone = False
        , files = []
        , uid = 0
        , uploadImageEndpoint = opts.uploadImageEndpoint
        , bgColor = opts.bgColor
        , color = opts.color
        }


destructiveRed : Color
destructiveRed =
    rgb255 209 11 27


type alias FileUpload =
    { file : File
    , progress : Progress
    , tracker : String
    }


type Progress
    = InProgress Float
    | Failed String


type Msg image
    = ReceivedFile image
    | PickerChanged (InnerMsg image)


type InnerMsg image
    = GotFiles File (List File)
    | OpenSelectImagesDialog
    | StartDropZoneHighlight
    | StopDropZoneHighlight
    | DismissFile String
    | UploadedFile String (Data image)
    | UpdateUploadProgress String Http.Progress


update : auth -> InnerMsg image -> Model auth image -> ( Model auth image, Cmd (Msg image) )
update auth msg (Model model) =
    Tuple.mapFirst Model <|
        case msg of
            StartDropZoneHighlight ->
                ( { model | highlightDropzone = True }, Cmd.none )

            StopDropZoneHighlight ->
                ( { model | highlightDropzone = False }, Cmd.none )

            OpenSelectImagesDialog ->
                ( { model | highlightDropzone = False }
                , Cmd.map PickerChanged (File.Select.files Mime.images GotFiles)
                )

            GotFiles file files ->
                let
                    ( progresses, newUid ) =
                        (file :: files)
                            |> List.foldl
                                (\f ( acc, uid ) ->
                                    let
                                        fileWithProgress =
                                            { file = f
                                            , progress =
                                                if List.member (File.mime f) Mime.images then
                                                    InProgress 0

                                                else
                                                    Failed "Only jpeg and png are supported"
                                            , tracker = File.name f ++ String.fromInt uid
                                            }
                                    in
                                    ( fileWithProgress :: acc
                                    , uid + 1
                                    )
                                )
                                ( [], model.uid )
                in
                ( { model
                    | files = progresses ++ model.files
                    , highlightDropzone = False
                    , uid = newUid
                  }
                , progresses
                    |> List.map
                        (\uploadableFile ->
                            case uploadableFile.progress of
                                InProgress _ ->
                                    case model.uploadImageEndpoint of
                                        HttpUpload endpoint ->
                                            endpoint auth
                                                { file = uploadableFile.file
                                                , tracker = uploadableFile.tracker
                                                , onResponse = UploadedFile uploadableFile.tracker
                                                }

                                        PortUpload portFn ->
                                            portFn uploadableFile.tracker uploadableFile.file

                                Failed _ ->
                                    Cmd.none
                        )
                    |> Cmd.batch
                    |> Cmd.map PickerChanged
                )

            UploadedFile tracker data ->
                let
                    files =
                        model.files
                in
                ( { model
                    | files =
                        files
                            |> List.map
                                (\file ->
                                    if file.tracker == tracker then
                                        case ( file.progress, data ) of
                                            ( InProgress _, Data.Failure error ) ->
                                                { file | progress = Failed ("Unable to complete upload: " ++ Data.stringForGenericErrors error) }

                                            ( _, Data.Success _ ) ->
                                                { file | progress = InProgress 1 }

                                            _ ->
                                                file

                                    else
                                        file
                                )
                  }
                , data
                    |> Data.toMaybe
                    |> Maybe.map (\image -> Process.sleep 30 |> Task.perform (\_ -> ReceivedFile image))
                    |> Maybe.withDefault Cmd.none
                )

            DismissFile tracker ->
                ( { model
                    | files =
                        model.files
                            |> List.filter (\f -> f.tracker /= tracker)
                  }
                , Cmd.none
                )

            UpdateUploadProgress tracker (Http.Sending progress) ->
                ( { model
                    | files =
                        model.files
                            |> List.map
                                (\file ->
                                    if tracker == file.tracker then
                                        case file.progress of
                                            InProgress _ ->
                                                { file
                                                    | progress =
                                                        InProgress (toFloat progress.sent / toFloat progress.size)
                                                }

                                            _ ->
                                                file

                                    else
                                        file
                                )
                  }
                , Cmd.none
                )

            UpdateUploadProgress _ (Http.Receiving _) ->
                ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model auth image -> Sub (Msg image)
subscriptions (Model model) =
    Sub.batch
        (model.files
            |> List.filterMap
                (\file ->
                    case file.progress of
                        InProgress _ ->
                            Just (Http.track file.tracker (UpdateUploadProgress file.tracker))

                        Failed _ ->
                            Nothing
                )
            |> List.map (Sub.map PickerChanged)
        )


viewDropzone : Model auth image -> Element (Msg image)
viewDropzone (Model model) =
    viewCustomDropzone (Model model) (el [ Font.color model.color, Font.size 21, Font.bold ] (text "Upload an image"))


viewCustomDropzone : Model auth image -> Element (InnerMsg image) -> Element (Msg image)
viewCustomDropzone (Model model) text_ =
    let
        dropDecoder : Json.Decode.Decoder (InnerMsg image)
        dropDecoder =
            Json.Decode.at [ "dataTransfer", "files" ] (Json.Decode.oneOrMore GotFiles File.decoder)

        hijackOn : String -> Json.Decode.Decoder msg -> Html.Attribute msg
        hijackOn event decoder =
            Html.Events.preventDefaultOn event (Json.Decode.map hijack decoder)

        hijack : msg -> ( msg, Bool )
        hijack msg =
            ( msg, True )
    in
    Element.map PickerChanged <|
        el
            [ height (px 75)
            , width fill
            , pointer
            , Element.Events.onClick OpenSelectImagesDialog
            , Border.rounded 8
            , Background.color
                (Colors.withAlpha
                    (if model.highlightDropzone then
                        0.4

                     else
                        0.3
                    )
                    model.bgColor
                )

            -- , Element.Events.onMouseEnter StartDropZoneHighlight
            -- , Element.Events.onMouseLeave StopDropZoneHighlight
            , htmlAttribute <| hijackOn "dragenter" (Json.Decode.succeed StartDropZoneHighlight)
            , htmlAttribute <| hijackOn "dragover" (Json.Decode.succeed StartDropZoneHighlight)
            , htmlAttribute <| hijackOn "dragleave" (Json.Decode.succeed StopDropZoneHighlight)
            , htmlAttribute (hijackOn "drop" dropDecoder)
            , behindContent
                --
                (html <|
                    Svg.svg
                        [ Svg.Attributes.width "100%"
                        , Svg.Attributes.height "100%"
                        , Svg.Attributes.overflow "visible"

                        -- , hijackOn "drop" dropDecoder
                        ]
                        [ Svg.rect
                            [ Svg.Attributes.width "100%"
                            , Svg.Attributes.height "100%"
                            , Svg.Attributes.fillOpacity "0"
                            , Svg.Attributes.strokeWidth "2px"
                            , Svg.Attributes.stroke (colorToString model.bgColor)
                            , Svg.Attributes.ry "8"
                            , Svg.Attributes.rx "8"
                            , Svg.Attributes.strokeDasharray "4, 8"

                            -- , Svg.Attributes.strokeDasharray "10, 12"
                            , Svg.Attributes.strokeLinecap "round"
                            ]
                            []
                        ]
                )
            ]
            (row [ spacing 10, centerX, centerY ]
                [ customIcon FeatherIcons.image 24 model.color
                , text_
                ]
            )


viewProgresses : Model auth image -> Element (Msg image)
viewProgresses (Model model) =
    Element.map PickerChanged <|
        column [ width fill, spacing 8 ]
            (el [] none
                :: List.map
                    (\file ->
                        case file.progress of
                            InProgress progress ->
                                el
                                    [ width fill
                                    , Background.color (Colors.withAlpha 0.4 model.bgColor)
                                    , Border.rounded 8
                                    , Border.width 1
                                    , Border.color model.bgColor
                                    , inFront
                                        (el
                                            [ height (px 4)
                                            , Html.Attributes.style "width" (String.fromInt (round (100 * progress)) ++ "%")
                                                |> htmlAttribute
                                            , Background.color (Colors.withAlpha 0.4 model.bgColor)
                                            , alignBottom
                                            ]
                                            none
                                        )
                                    , padding 12
                                    ]
                                    (row [ centerY, width fill ]
                                        [ column [ spacing 4 ]
                                            [ if round (progress * 100) == 100 then
                                                text "Done!"

                                              else
                                                text (String.fromInt (round (100 * progress)) ++ "%")
                                            , el [ Font.size 13 ] (text (File.name file.file))
                                            ]
                                        , if round (progress * 100) == 100 then
                                            Input.button [ alignRight ]
                                                { label = customIcon FeatherIcons.x 24 model.color
                                                , onPress = Just (DismissFile file.tracker)
                                                }

                                          else
                                            none
                                        ]
                                    )

                            Failed error ->
                                el
                                    [ width fill
                                    , Background.color (Colors.withAlpha 0.3 Colors.red)
                                    , Border.rounded 8
                                    , Border.width 1
                                    , Border.color Colors.red
                                    , padding 12
                                    ]
                                    (row [ centerY, width fill ]
                                        [ column [ spacing 4 ]
                                            [ text error
                                            , el [ Font.size 13 ] (text (File.name file.file))
                                            ]
                                        , Input.button [ alignRight ]
                                            { label = customIcon FeatherIcons.x 24 destructiveRed
                                            , onPress = Just (DismissFile file.tracker)
                                            }
                                        ]
                                    )
                    )
                    model.files
            )


customIcon : FeatherIcons.Icon -> Float -> Color -> Element msg
customIcon icon size iconColor =
    icon
        |> FeatherIcons.withSize size
        |> FeatherIcons.withStrokeWidth 2.4
        |> FeatherIcons.toHtml []
        |> html
        |> el [ Font.color iconColor ]


colorToString : Color -> String
colorToString color =
    let
        o =
            toRgb color
    in
    "rgba("
        ++ String.fromInt (round (o.red * 255))
        ++ ("," ++ String.fromInt (round (o.green * 255)))
        ++ ("," ++ String.fromInt (round (o.blue * 255)))
        ++ ("," ++ String.fromFloat o.alpha)
        ++ ")"
