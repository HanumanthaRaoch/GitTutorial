module SocketChannel exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push
import Json.Encode as JsEncode
import Json.Decode as JsDecode


type alias Model =
    { phxSocket : Phoenix.Socket.Socket Msg
    , messageInPrograess : String
    , message : List String
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { phxSocket = Phoenix.Socket.init "ws://localhost:4000/socket/websocket"
            , messageInPrograess = ""
            , message = [ "Test message" ]
            }
    in
        ( model, Cmd.none )


drawMessage : String -> Html Msg
drawMessage message =
    li []
        [ text message
        ]


type Msg
    = PhoenixMsg (Phoenix.Socket.Msg Msg)
    | SetMessage String
    | SendMessage
    | Receivemessage JsEncode.Value
    | HandleSendError JsEncode.Value
    | ReceiveMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )

        SetMessage message ->
            ( { model | messageInProgress = message }, Cmd.none )

        SendMessage ->
            let
                payload =
                    JsEncode.object
                        [ ( "message", JsEncode.string model.messageInProgress )
                        ]

                phxPush =
                    Phoenix.Push.init "shout" "room:lobby"
                        |> Phoenix.Push.withPayload payload
                        |> Phoenix.Push.onOk ReceiveMessage
                        |> Phoenix.Push.onError HandleSendError

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.push phxPush model.phxSocket
            in
                ( { model
                    | phxSocket = phxSocket
                  }
                , Cmd.map PhoenixMsg phxCmd
                )

        ReceiveMessage _ ->
            ( model, Cmd.none )

        HandleSendError _ ->
            let
                message =
                    "Failed to Send Message"
            in
                ( { model | messages = message :: model.messages }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Phoenix.Socket.listen model.phxSocket PhoenixMsg


view : Model -> Html Msg
view model =
    let
        drawMessages messages =
            messages |> List.map drawMessage
    in
        div []
            [ li [] (model.messages |> drawMessages)
            , form []
                [ input [ onInput SetMessage ]
                    []
                , button []
                    [ text "Submit"
                    ]
                ]
            ]
