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
        setDiff : Set.Set Char  -- Set of Chars in input not in valid set
        setDiff =
            validChars
            |> Set.diff (Set.fromList <| String.toList <| String.toUpper x)

        badChars : String  -- Comma-separated String from Set of Chars
        badChars =
            String.join ", " <| List.map String.fromChar <| Set.toList setDiff
    in
        if Set.isEmpty setDiff then  -- If empty, input has no illegal chars
            Nothing
        else
            if String.length badChars > 1 then
                Just ("String contains illegal characters: " ++ badChars)
            else  -- Only one bad character
                Just ("String contains an illegal character: " ++ badChars)


{-| Translates a capitalized string "JERK" to Meowth speak

Returns an unaltered string otherwise
-}
toJoik : String -> String
toJoik x =
    if x == "JERK" then
        "JOIK"
    else
        x


{-| Translates a capitalized string "YOU" to Meowth speak

Returns an unaltered string otherwise
-}
toYous : String -> String
toYous x =
    if x == "YOU" then
        "YOUS"
    else
        x


{-| Translates a capitalized string ending in "ING" to Meowth speak

Returns an unaltered string otherwise
-}
toIn : String -> String
toIn x =
    if (String.endsWith "ING" x) then
        String.append (String.dropRight 1 x) "'"
    else
        x


{-| Translates a capitalized string starting with "TH" to Meowth speak

Returns an unaltered string otherwise
-}
toDe : String -> String
toDe x =
    if (String.startsWith "TH" x) then
        String.cons 'D' <| String.dropLeft 2 x
    else
        x

{-| Translates a capitalized string "YOUR" or "YOURE" to Meowth speak

Returns an unaltered string otherwise
-}
toYer : String -> String
toYer x =
    if x == "YOUR" || x == "YOURE" then
        "YER"
    else
        x


{-| Attempt to translate a `String` to Meowth speak.

Returns `Ok String` with the translated string if everything went
well.

Returns `Err String` when the input string is invalid.
-}
meowthify : String -> Result String String
meowthify s =
    let
        newStr : String
        newStr = s
                 |> String.toUpper  -- Rule 1: Meowth likes to yell
                 |> String.words  -- Split input to parse
                 |> List.map (\x -> toJoik x)  -- Rule 2
                 |> List.map (\x -> toYous x)  -- Rule 3
                 |> List.map (\x -> toIn x)  -- Rule 4
                 |> List.map (\x -> toDe x)  -- Rule 5
                 |> List.map (\x -> toYer x)  -- Rule 6
                 |> String.join " "  -- Translated string
    in
        case (validString s) of
            Just x -> Err x
            Nothing -> Ok newStr


{-| Insert the (uncertain)phrase "YANNO WHAT I MEAN" into a `String`
at a given index. The phrase is always inserted between words.

If the index is outside the range of the input `String` (either less
than 0 or greater than the number of words in the input), the phrase
is inserted at the beginning or the end of the `String`.
-}
addUncertainty : Int -> String -> String
addUncertainty i string =
    let
        uncertain : List String
        uncertain =
            [ "YANNO WHAT I MEAN" ]

        (leftSide, rightSide) = string  -- Breaks up string into two parts
                                |> String.words  -- Break into words
                                |> List.indexedMap (,)  -- List of tuples
                                |> List.partition (\(index, str) -> index < i)

        leftString : List String  -- Words left of uncertainty string
        leftString =
            List.map (\(index, str) -> str) leftSide

        rightString : List String  -- Words right of uncertainty string
        rightString =
            List.map (\(index, str) -> str) rightSide
    in
        String.join " "
            <| List.concat [ leftString, uncertain, rightString ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewContent content ->
            case (meowthify content) of
                Ok x ->
                    ( { model
                        | translated = x
                        , errorMsg = ""
                      }
                    , Cmd.none
                    )
                Err y ->
                    ( { model
                        | translated = ""
                        , errorMsg = y
                      }
                    , Cmd.none
                    )


        AddUncertainty ->
            let
                w : Int  -- Number of words in input
                w =
                    List.length <| String.words model.translated
            in
                ( model, Random.generate RandomIndex (Random.int 0 w))

        RandomIndex i ->
            ( { model
                | translated = addUncertainty i model.translated
                , errorMsg = model.errorMsg
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
