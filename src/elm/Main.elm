import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (Decoder, at, string)

-- custom imports
import Constants
import Bar
import Foo

-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

type QuoteLoad
  = Failure
  | Loading
  | Success String

type alias Model =
    { quoteStatus: QuoteLoad,
      index: Int
    }

init : () -> (Model, Cmd Msg)
init _ =
  ({quoteStatus = Loading, index = 1}, generateQuote)


-- UPDATE


type Msg
  = MorePlease
  | GotQuote (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      ({quoteStatus = Loading, index = model.index + 1}, generateQuote)

    GotQuote result ->
      case result of
        Ok joke ->
          ({quoteStatus = Success joke, index = model.index}, Cmd.none)

        Err _ ->
          ({quoteStatus = Failure, index = model.index}, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Chuck Norris Quotes" ]
    , viewGif model
    ]


viewGif : Model -> Html Msg
viewGif model =
  case model.quoteStatus of
    Failure ->
      div []
        [ text Constants.failureText
        , button [ onClick MorePlease ] [ text "Try Again!" ]
        ]

    Loading ->
      text Constants.loadingText

    Success joke ->
      div []
        [ div 
            [ classList
                [ ("chuck-norris-fact", True)
                ]
            ]
            [ text joke ]
        , button [ onClick MorePlease, style "display" "block" ] [ text Constants.moreQuoteText ]
        , div [] [ Foo.foo ]
        , div [] [ Bar.bar ]
        , div [] [ text (String.fromInt model.index) ]
        ]



-- HTTP


generateQuote : Cmd Msg
generateQuote =
  Http.get 
    { url = Constants.quoteURL
    , expect = Http.expectJson GotQuote quoteDecoder
    }

quoteDecoder : Decoder String
quoteDecoder =
    at ["value", "joke"] string