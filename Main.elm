import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Random
import Set
import String
import Tuple


-- CONSTANTS


{-| Set of allowed input characters
-}
validChars : Set.Set Char
validChars =
    Set.fromList <| String.toList " ABCDEFGHIJKLMNOPQRSTUVWXYZ"



-- MAIN


main =
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }



-- INIT


type alias Model =
    { translated : String  -- The translated message for display
    , errorMsg : String     -- The error message (if any)
    }


init : ( Model, Cmd Msg )
init =
    let
        m =
            { translated = ""
            , errorMsg = ""
            }
    in
        ( m, Cmd.none )



-- UPDATE


type Msg
    = NewContent String  -- The user has typed in the text area
    | AddUncertainty     -- The user has pressed the "Add Uncertainty" button
    | RandomIndex Int    -- The Elm Runtime has chosen a random index for you


{-| Determine if a string is valid.

Returns `Nothing` if there was no problem and the string is valid.

Returns `Just String` if there was a problem. The corresponding
`String` contains the invalid characters (separated by ", ").
-}
validString : String -> Maybe String
validString x =
    let
        setDiff : Set.Set Char  -- Set of Chars in input string that aren't in valid set
        setDiff =
            Set.diff (Set.fromList <| String.toList (String.toUpper x)) validChars
    in
        if Set.isEmpty setDiff then  -- If empty, input contains no illegal chars
            Nothing
        else
            let
                badChars : String  -- List of Strings converted from Set of Chars
                badChars =
                    String.join ", " (List.map String.fromChar (Set.toList setDiff))

                badChar : String  -- For case of only one invalid Char
                badChar =
                    String.fromList (Set.toList setDiff)
            in
                if List.length badChars > 1 then
                    Just ("String contains illegal characters: " ++ badChars)
                else  -- Only one bad character
                    Just ("String contains an illegal character: " ++ badChar)


{-| Attempt to translate a `String` to Meowth speak.

Returns `Ok String` with the translated string if everything went
well.

Returns `Err String` when the input string is invalid.
-}
meowthify : String -> Result String String
meowthify s =
    case (validString s) of
        Just x -> Err x
        Nothing -> Ok (s
                       |> String.toUpper  -- Rule 1: Meowth likes to yell
                       |> String.words  -- Split input to parse
                       |> List.map (\x -> if x == "JERK" then "JOIK" else x)
                       |> List.map (\x -> if x == "YOU" then "YOUS" else x)
                       |> List.map (\x -> if (String.endsWith "ING" x) then String.append (String.dropRight 1 x) "'" else x)
                       |> List.map (\x -> if (String.startsWith "TH" x) then String.cons 'D' (String.dropLeft 2 x) else x)
                       |> List.map (\x -> if x == "YOUR" || x == "YOURE" then "YER" else x)
                       |> String.join " "  -- Translated string
                       )


{-| Insert the (uncertain)phrase "YANNO WHAT I MEAN" into a `String`
at a given index. The phrase is always inserted between words.

If the index is outside the range of the input `String` (either less
than 0 or greater than the number of words in the input), the phrase
is inserted at the beginning or the end of the `String`.
-}
addUncertainty : Int -> String -> String
addUncertainty i string =
    String.join " " <| List.concat [ [ "YANNO WHAT I MEAN" ], [ string ] ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewContent content ->
            ( { model  -- Update the current state
                | translated =
                      case (meowthify content) of
                          Ok x ->
                              x  -- No error, output tranlated input

                          Err y ->
                              "" -- Error found, but it goes to errorMsg
                , errorMsg =
                      case (meowthify content) of
                          Ok x ->
                              ""  -- No problems here

                          Err y ->
                              y  -- Something is wrong
              }
            , Cmd.none
            )

        AddUncertainty ->
            ( model, Random.generate RandomIndex (Random.int 0 99))

        RandomIndex i ->
            ( { model
                | translated = addUncertainty i translated
                , errorMsg = ""
              }
            , Cmd.none )



-- VIEW


{-| Convert a Model into a renderable HTML value.

This function requires no modification.
-}
view : Model -> Html.Html Msg
view model =
    Html.div
        []
        [ Html.textarea
            [ Attributes.placeholder "Text to translate"
            , Events.onInput NewContent
            ]
            []
        , Html.button
            [ Events.onClick AddUncertainty ]
            [ Html.text "Add Uncertainty" ]
        , Html.div
            []
            [ Html.p
                []
                [ Html.text model.translated ]
            , Html.p
                [ Attributes.style [ ( "color", "red" ) ] ]
                [ Html.text model.errorMsg ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
