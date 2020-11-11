module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D exposing (Decoder)
import Url.Builder exposing (QueryParameter, crossOrigin)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


type alias Translation =
    ( String, String )


type alias Model =
    { query : String
    , fromLang : String
    , toLang : String
    , translations : List Translation
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( Model "" "pt" "en" []
    , Cmd.none
    )


type Msg
    = ChangeQuery String
    | ChangeFromLang String
    | ChangeToLang String
    | ClickButton
    | GotSearchResults (Result Http.Error (List String))
    | GotTranslations (Result Http.Error (List ( String, List LangLink )))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeQuery s ->
            ( { model | query = s }
            , Cmd.none
            )

        ChangeFromLang s ->
            ( { model | fromLang = s }
            , Cmd.none
            )

        ChangeToLang s ->
            ( { model | toLang = s }
            , Cmd.none
            )

        ClickButton ->
            ( model
            , searchTitles model.fromLang model.query
            )

        GotSearchResults response ->
            ( model
            , translateTitles
                (Result.withDefault [] response)
                model.fromLang
                model.toLang
            )

        GotTranslations response ->
            let
                mapToSecond : (b -> Maybe c) -> ( a, b ) -> Maybe ( a, c )
                mapToSecond f ( x, y ) =
                    case f y of
                        Just z ->
                            Just ( x, z )

                        Nothing ->
                            Nothing

                listFind : (a -> Bool) -> List a -> Maybe a
                listFind predicate list =
                    List.head (List.filter predicate list)

                filterLanguage : ( String, List LangLink ) -> Maybe ( String, LangLink )
                filterLanguage =
                    mapToSecond (listFind (\l -> l.lang == model.toLang))

                filterResults : List ( String, List LangLink ) -> List ( String, LangLink )
                filterResults =
                    List.filterMap filterLanguage

                exposeTitles : List ( String, LangLink ) -> List ( String, String )
                exposeTitles =
                    List.map (\( title, langLink ) -> ( title, langLink.title ))

                results =
                    Result.withDefault [] response
            in
            ( { model | translations = exposeTitles (filterResults results) }
            , Cmd.none
            )


apiUrl : String -> List ( String, String ) -> String
apiUrl lang params =
    let
        tupleToParam ( k, v ) =
            Url.Builder.string k v

        baseParams =
            [ ( "origin", "*" ), ( "format", "json" ) ]
    in
    crossOrigin
        ("https://" ++ lang ++ ".wikipedia.org")
        [ "w", "api.php" ]
        (List.map tupleToParam (baseParams ++ params))


searchTitles : String -> String -> Cmd Msg
searchTitles fromLang query =
    let
        titlesDecoder =
            D.field "1" (D.list D.string)

        parameters =
            [ ( "action", "opensearch" )
            , ( "namespace", "0" )
            , ( "search", query )
            ]
    in
    Http.get
        { url = apiUrl fromLang parameters
        , expect = Http.expectJson GotSearchResults titlesDecoder
        }


type alias PageId =
    String


type alias PageInfo =
    { title : String, langLinks : List LangLink }


type alias LangLink =
    { lang : String, title : String }


translateTitles : List String -> String -> String -> Cmd Msg
translateTitles titles fromLang toLang =
    let
        objectValues : Decoder a -> Decoder (List a)
        objectValues d =
            D.map
                (List.map (\( k, v ) -> v))
                (D.keyValuePairs d)

        langLinkDecoder : Decoder LangLink
        langLinkDecoder =
            D.map2
                LangLink
                (D.field "lang" D.string)
                (D.field "*" D.string)

        pageInfoDecoder : Decoder ( String, List LangLink )
        pageInfoDecoder =
            D.map2
                (\title -> \translation -> ( title, translation ))
                (D.field "title" D.string)
                (D.map
                    (\m -> Maybe.withDefault [] m)
                    (D.maybe (D.field "langlinks" (D.list langLinkDecoder)))
                )

        baseDecoder : Decoder (List ( String, List LangLink ))
        baseDecoder =
            D.at [ "query", "pages" ] (objectValues pageInfoDecoder)

        parameters =
            [ ( "action", "query" )
            , ( "prop", "langlinks" )
            , ( "lllang", toLang )
            , ( "titles", String.join "|" titles )
            ]
    in
    Http.get
        { url = apiUrl fromLang parameters
        , expect = Http.expectJson GotTranslations baseDecoder
        }


viewTranslation : Translation -> Html msg
viewTranslation ( from, to ) =
    p [] [ text (from ++ " -> " ++ to) ]


view : Model -> Html Msg
view model =
    div [] (form model :: List.map viewTranslation model.translations)


form : Model -> Html Msg
form model =
    p []
        [ text "translate"
        , input [ value model.query, onInput ChangeQuery ] []
        , text "from"
        , input [ value model.fromLang, onInput ChangeFromLang ] []
        , text "to"
        , input [ value model.toLang, onInput ChangeToLang ] []
        , button [ onClick ClickButton ] [ text "go" ]
        ]
