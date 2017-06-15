module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode
import Json.Decode.Pipeline
import Json.Decode.Extra


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type ApiStatus
    = NotFetchedYet
    | Fetching
    | Done
    | Error


type alias Model =
    { metaThings : Maybe MetaThings
    , metaThingsApiError : Maybe Http.Error
    , metaThingsApiUrl : String
    , metaThingsApiStatus : ApiStatus
    }


init : ( Model, Cmd Msg )
init =
    ( { metaThings = Nothing
      , metaThingsApiError = Nothing
      , metaThingsApiUrl = "https://api.jsoneditoronline.org/v1/docs/3e4dbce6f34017948c0a5ea43bcaf141"
      , metaThingsApiStatus = NotFetchedYet
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NewMetaThingsData (Result Http.Error MetaThings)
    | FetchMetaThingsData
    | ChangeMetaThingsApi String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewMetaThingsData result ->
            case result of
                Ok data ->
                    ( { model | metaThingsApiStatus = Done, metaThings = Just data }, Cmd.none )

                Err data ->
                    ( { model | metaThingsApiStatus = Error, metaThingsApiError = Just data }, Cmd.none )

        FetchMetaThingsData ->
            ( { model | metaThingsApiStatus = Fetching, metaThings = Nothing, metaThingsApiError = Nothing }, Http.send NewMetaThingsData (Http.get model.metaThingsApiUrl metaThings) )

        ChangeMetaThingsApi newUrl ->
            ( { model | metaThingsApiUrl = newUrl }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Decoding Json that contain Json" ]
        , input
            [ onInput ChangeMetaThingsApi
            , value model.metaThingsApiUrl
            , style
                [ ( "font-size", "large" )
                , ( "padding", "0.3em" )
                , ( "width", "50%" )
                , ( "min-width", "200px" )
                ]
            ]
            []
        , button
            [ onClick FetchMetaThingsData
            , style [ ( "font-size", "large" ), ( "padding", "0.3em" ) ]
            ]
            [ text ("Fetch Data") ]
        , div [ style [ ( "color", "#aaa" ) ] ]
            [ text ("Url: " ++ (model.metaThingsApiUrl)) ]
        , div []
            [ text ("Status: " ++ (toString model.metaThingsApiStatus)) ]
        , div
            [ style [ ( "color", "green" ) ] ]
            [ text ("Ok: " ++ (toString model.metaThings)) ]
        , div
            [ style [ ( "color", "red" ) ] ]
            [ text ("Error: " ++ (toString model.metaThingsApiError)) ]
        , h2 [] [ text "Json editor online" ]
        , div [ style [ ( "color", "#aaa" ) ] ]
            [ a
                [ target "_blank"
                , href "http://www.jsoneditoronline.org/?id=3e4dbce6f34017948c0a5ea43bcaf141"
                ]
                [ text "Edit Json" ]
            ]
        , div [] [ text "In case the Json file is broken, replace it with:" ]
        , pre []
            [ text """
{ "things":
  [
    {
      "id": "nature",
      "name": "Nature"
    },
    {
      "id": "sport",
      "name": "Sport"
    }
  ]
}
"""
            ]
        ]



-- META JSON


type alias Thing =
    { id : String
    , name : String
    }


type alias MetaThings =
    { id : String
    , rev : String
    , name : String
    , data : List Thing
    , updated : String
    }


metaThings : Json.Decode.Decoder MetaThings
metaThings =
    Json.Decode.Pipeline.decode MetaThings
        |> Json.Decode.Pipeline.required "_id" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "_rev" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "data" (Json.Decode.Extra.doubleEncoded things)
        |> Json.Decode.Pipeline.required "updated" (Json.Decode.string)


things : Json.Decode.Decoder (List Thing)
things =
    Json.Decode.at [ "things" ] (Json.Decode.list thing)


thing : Json.Decode.Decoder Thing
thing =
    Json.Decode.Pipeline.decode Thing
        |> Json.Decode.Pipeline.required "id" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "name" (Json.Decode.string)
