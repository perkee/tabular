module Frontend exposing (..)

import Browser
import Dict exposing (Dict)
import Effect.Browser.Navigation as Nav
import Effect.Command as Command exposing (Command, FrontendOnly)
import Effect.Lamdera
import Effect.Subscription as Subscription
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import Svg
import Svg.Attributes as SvgAttr
import Lamdera
import SeqSet
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Effect.Lamdera.frontend
        Lamdera.sendToBackend
        app_


app_ =
    { init = init
    , onUrlRequest = UrlClicked
    , onUrlChange = UrlChanged
    , update = update
    , updateFromBackend = updateFromBackend
    , subscriptions = \_ -> Subscription.none
    , view = view
    }


init : Url.Url -> Nav.Key -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
init _ key =
    ( { key = key
      , rows = 3
      , cols = 3
      , cells =
            Dict.fromList
                [ ( ( 0, 0 ), "Header 1" )
                , ( ( 0, 1 ), "Header 2" )
                , ( ( 0, 2 ), "Header 3" )
                ]
      , headerAlignments = Dict.empty
      , bodyAlignments = Dict.empty
      , horizontalLineStyles = Dict.empty
      , verticalLineStyles = Dict.empty
      , cellHorizontalStyles = Dict.empty
      , cellVerticalStyles = Dict.empty
      , outputFormat = Expanded
      , showImport = False
      , importText = ""
      , collapsedSections = SeqSet.empty
      , undoStack = []
      , sortState = Unsorted
      , summaryRows = SeqSet.empty
      , summarySeparatorStyles = Dict.empty
      }
    , Command.none
    )


snapshot : Model -> TableSnapshot
snapshot model =
    { rows = model.rows
    , cols = model.cols
    , cells = model.cells
    , headerAlignments = model.headerAlignments
    , bodyAlignments = model.bodyAlignments
    , horizontalLineStyles = model.horizontalLineStyles
    , verticalLineStyles = model.verticalLineStyles
    , cellHorizontalStyles = model.cellHorizontalStyles
    , cellVerticalStyles = model.cellVerticalStyles
    }


pushUndo : Model -> List TableSnapshot
pushUndo model =
    snapshot model :: model.undoStack |> List.take 50


extractNumeric : String -> Maybe Float
extractNumeric s =
    let
        filtered =
            String.filter (\ch -> Char.isDigit ch || ch == '.') s
    in
    if String.isEmpty filtered then
        Nothing

    else
        String.toFloat filtered


summaryLabel : SummaryFunction -> String
summaryLabel fn =
    case fn of
        SummaryMax ->
            "MAX"


formatSummaryValue : Float -> String
formatSummaryValue f =
    if f == toFloat (round f) then
        String.fromInt (round f)

    else
        String.fromFloat f


computeSummaryRow : SummaryFunction -> Int -> Dict ( Int, Int ) String -> List Int -> List String
computeSummaryRow fn cols cells bodyRowIndices =
    let
        colRange =
            List.range 0 (cols - 1)

        computeCol c =
            if c == 0 then
                summaryLabel fn

            else
                let
                    values =
                        List.filterMap
                            (\r -> extractNumeric (getCell r c cells))
                            bodyRowIndices
                in
                case values of
                    [] ->
                        ""

                    _ ->
                        case fn of
                            SummaryMax ->
                                formatSummaryValue (List.foldl max (Maybe.withDefault 0 (List.head values)) values)
    in
    List.map computeCol colRange


sortedBodyRows : Model -> List Int
sortedBodyRows model =
    computeSortedBodyRows model.rows model.cells model.sortState


computeSortedBodyRows : Int -> Dict ( Int, Int ) String -> SortState -> List Int
computeSortedBodyRows rows cells sortState =
    let
        bodyRange =
            List.range 1 (rows - 1)
    in
    case sortState of
        Unsorted ->
            bodyRange

        SortedBy col dir method ->
            let
                getValue r =
                    getCell r col cells

                reverseOrder o =
                    case o of
                        LT ->
                            GT

                        EQ ->
                            EQ

                        GT ->
                            LT

                applyDir o =
                    case dir of
                        Ascending ->
                            o

                        Descending ->
                            reverseOrder o

                compareRows r1 r2 =
                    case method of
                        Lexicographic ->
                            applyDir (compare (getValue r1) (getValue r2))

                        Numeric ->
                            case ( extractNumeric (getValue r1), extractNumeric (getValue r2) ) of
                                ( Just a, Just b ) ->
                                    applyDir (compare a b)

                                ( Just _, Nothing ) ->
                                    LT

                                ( Nothing, Just _ ) ->
                                    GT

                                ( Nothing, Nothing ) ->
                                    EQ
            in
            List.sortWith compareRows bodyRange


update : FrontendMsg -> Model -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        UrlChanged _ ->
            ( model, Command.none )

        CellChanged row col value ->
            ( { model | cells = Dict.insert ( row, col ) value model.cells }
            , Command.none
            )

        AddRow ->
            ( { model | rows = model.rows + 1, undoStack = pushUndo model }, Command.none )

        AddColumn ->
            ( { model | cols = model.cols + 1, undoStack = pushUndo model }, Command.none )

        RemoveRow rowIndex ->
            if model.rows > 1 then
                ( { model
                    | rows = model.rows - 1
                    , cells = removeRow rowIndex model.cells
                    , horizontalLineStyles = removeIndexFromDict (rowIndex + 1) model.horizontalLineStyles
                    , cellHorizontalStyles = removeCellStyleRow rowIndex model.cellHorizontalStyles
                    , cellVerticalStyles = removeCellVStyleRow rowIndex model.cellVerticalStyles
                    , undoStack = pushUndo model
                  }
                , Command.none
                )

            else
                ( model, Command.none )

        RemoveColumn colIndex ->
            if model.cols > 1 then
                let
                    newSortState =
                        case model.sortState of
                            SortedBy sortCol dir method ->
                                if sortCol == colIndex then
                                    Unsorted

                                else if sortCol > colIndex then
                                    SortedBy (sortCol - 1) dir method

                                else
                                    model.sortState

                            Unsorted ->
                                Unsorted
                in
                ( { model
                    | cols = model.cols - 1
                    , cells = removeColumn colIndex model.cells
                    , headerAlignments = removeColumnAlignments colIndex model.headerAlignments
                    , bodyAlignments = removeColumnAlignments colIndex model.bodyAlignments
                    , verticalLineStyles = removeIndexFromDict (colIndex + 1) model.verticalLineStyles
                    , cellHorizontalStyles = removeCellStyleCol colIndex model.cellHorizontalStyles
                    , cellVerticalStyles = removeCellVStyleCol colIndex model.cellVerticalStyles
                    , undoStack = pushUndo model
                    , sortState = newSortState
                  }
                , Command.none
                )

            else
                ( model, Command.none )

        InsertRow index ->
            ( { model
                | rows = model.rows + 1
                , cells = insertRow index model.cells
                , horizontalLineStyles = insertIndexIntoDict (index + 1) model.horizontalLineStyles
                , cellHorizontalStyles = insertCellStyleRow index model.cellHorizontalStyles
                , cellVerticalStyles = insertCellVStyleRow index model.cellVerticalStyles
                , undoStack = pushUndo model
              }
            , Command.none
            )

        InsertColumn index ->
            let
                newSortState =
                    case model.sortState of
                        SortedBy sortCol dir method ->
                            if sortCol >= index then
                                SortedBy (sortCol + 1) dir method

                            else
                                model.sortState

                        Unsorted ->
                            Unsorted
            in
            ( { model
                | cols = model.cols + 1
                , cells = insertColumn index model.cells
                , headerAlignments = insertColumnAlignments index model.headerAlignments
                , bodyAlignments = insertColumnAlignments index model.bodyAlignments
                , verticalLineStyles = insertIndexIntoDict (index + 1) model.verticalLineStyles
                , cellHorizontalStyles = insertCellStyleCol index model.cellHorizontalStyles
                , cellVerticalStyles = insertCellVStyleCol index model.cellVerticalStyles
                , undoStack = pushUndo model
                , sortState = newSortState
              }
            , Command.none
            )

        SetOutputFormat format ->
            ( { model | outputFormat = format }, Command.none )

        SetHeaderAlignment col alignment ->
            ( { model | headerAlignments = Dict.insert col alignment model.headerAlignments }
            , Command.none
            )

        SetBodyAlignment col alignment ->
            ( { model | bodyAlignments = Dict.insert col alignment model.bodyAlignments }
            , Command.none
            )

        ToggleImport ->
            ( { model | showImport = not model.showImport, importText = "" }
            , Command.none
            )

        ImportTextChanged value ->
            ( { model | importText = value }, Command.none )

        ImportData ->
            let
                parsed =
                    parseImportData model.importText
            in
            if parsed.rows > 0 && parsed.cols > 0 then
                ( { model
                    | rows = parsed.rows
                    , cols = parsed.cols
                    , cells = parsed.cells
                    , headerAlignments = Dict.empty
                    , bodyAlignments = Dict.empty
                    , horizontalLineStyles = Dict.empty
                    , verticalLineStyles = Dict.empty
                    , cellHorizontalStyles = Dict.empty
                    , cellVerticalStyles = Dict.empty
                    , showImport = False
                    , importText = ""
                    , undoStack = pushUndo model
                    , sortState = Unsorted
                  }
                , Command.none
                )

            else
                ( model, Command.none )

        CycleHorizontalLineStyle idx ->
            let
                current =
                    getHorizontalLineStyle idx model.horizontalLineStyles

                clearedCellStyles =
                    model.cellHorizontalStyles
                        |> Dict.filter (\( h, _ ) _ -> h /= idx)
            in
            ( { model
                | horizontalLineStyles = Dict.insert idx (cycleLineStyle current) model.horizontalLineStyles
                , cellHorizontalStyles = clearedCellStyles
              }
            , Command.none
            )

        CycleVerticalLineStyle idx ->
            let
                current =
                    getVerticalLineStyle idx model.verticalLineStyles

                clearedCellStyles =
                    model.cellVerticalStyles
                        |> Dict.filter (\( _, v ) _ -> v /= idx)
            in
            ( { model
                | verticalLineStyles = Dict.insert idx (cycleLineStyle current) model.verticalLineStyles
                , cellVerticalStyles = clearedCellStyles
              }
            , Command.none
            )

        CycleCellHorizontalStyle hIdx col ->
            let
                current =
                    getEffectiveHStyle hIdx col model.cellHorizontalStyles model.horizontalLineStyles
            in
            ( { model | cellHorizontalStyles = Dict.insert ( hIdx, col ) (cycleLineStyle current) model.cellHorizontalStyles }
            , Command.none
            )

        CycleCellVerticalStyle row vIdx ->
            let
                current =
                    getEffectiveVStyle row vIdx model.cellVerticalStyles model.verticalLineStyles
            in
            ( { model | cellVerticalStyles = Dict.insert ( row, vIdx ) (cycleLineStyle current) model.cellVerticalStyles }
            , Command.none
            )

        Undo ->
            case model.undoStack of
                s :: rest ->
                    let
                        newSortState =
                            case model.sortState of
                                SortedBy sortCol _ _ ->
                                    if sortCol >= s.cols then
                                        Unsorted

                                    else
                                        model.sortState

                                Unsorted ->
                                    Unsorted
                    in
                    ( { model
                        | rows = s.rows
                        , cols = s.cols
                        , cells = s.cells
                        , headerAlignments = s.headerAlignments
                        , bodyAlignments = s.bodyAlignments
                        , horizontalLineStyles = s.horizontalLineStyles
                        , verticalLineStyles = s.verticalLineStyles
                        , cellHorizontalStyles = s.cellHorizontalStyles
                        , cellVerticalStyles = s.cellVerticalStyles
                        , undoStack = rest
                        , sortState = newSortState
                      }
                    , Command.none
                    )

                [] ->
                    ( model, Command.none )

        SetSortColumn str ->
            let
                newSortState =
                    case String.toInt str of
                        Just col ->
                            if col >= 0 && col < model.cols then
                                SortedBy col Ascending Lexicographic

                            else
                                Unsorted

                        Nothing ->
                            Unsorted
            in
            ( { model | sortState = newSortState }, Command.none )

        SetSortDirection dir ->
            case model.sortState of
                SortedBy col _ method ->
                    ( { model | sortState = SortedBy col dir method }, Command.none )

                Unsorted ->
                    ( model, Command.none )

        SetSortMethod method ->
            case model.sortState of
                SortedBy col dir _ ->
                    ( { model | sortState = SortedBy col dir method }, Command.none )

                Unsorted ->
                    ( model, Command.none )

        ApplySortToInputs ->
            let
                bodyRowOrder =
                    sortedBodyRows model

                reorderedCells =
                    bodyRowOrder
                        |> List.indexedMap
                            (\i oldRow ->
                                let
                                    newRow =
                                        i + 1
                                in
                                List.range 0 (model.cols - 1)
                                    |> List.filterMap
                                        (\c ->
                                            case Dict.get ( oldRow, c ) model.cells of
                                                Just val ->
                                                    Just ( ( newRow, c ), val )

                                                Nothing ->
                                                    Nothing
                                        )
                            )
                        |> List.concat

                headerCells =
                    List.range 0 (model.cols - 1)
                        |> List.filterMap
                            (\c ->
                                case Dict.get ( 0, c ) model.cells of
                                    Just val ->
                                        Just ( ( 0, c ), val )

                                    Nothing ->
                                        Nothing
                            )
            in
            ( { model
                | cells = Dict.fromList (headerCells ++ reorderedCells)
                , sortState = Unsorted
                , undoStack = pushUndo model
              }
            , Command.none
            )

        ToggleSection section ->
            let
                newSections =
                    if SeqSet.member section model.collapsedSections then
                        SeqSet.remove section model.collapsedSections

                    else
                        SeqSet.insert section model.collapsedSections
            in
            ( { model | collapsedSections = newSections }, Command.none )

        ToggleSummaryRow fn ->
            let
                newSummaryRows =
                    if SeqSet.member fn model.summaryRows then
                        SeqSet.remove fn model.summaryRows

                    else
                        SeqSet.insert fn model.summaryRows
            in
            ( { model | summaryRows = newSummaryRows }, Command.none )

        CycleSummarySeparatorStyle idx ->
            let
                current =
                    Dict.get idx model.summarySeparatorStyles |> Maybe.withDefault Thin
            in
            ( { model | summarySeparatorStyles = Dict.insert idx (cycleLineStyle current) model.summarySeparatorStyles }
            , Command.none
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Command.none )



-- HELPERS


removeRow : Int -> Dict ( Int, Int ) String -> Dict ( Int, Int ) String
removeRow rowToRemove cells =
    cells
        |> Dict.toList
        |> List.filterMap
            (\( ( r, c ), v ) ->
                if r == rowToRemove then
                    Nothing

                else if r > rowToRemove then
                    Just ( ( r - 1, c ), v )

                else
                    Just ( ( r, c ), v )
            )
        |> Dict.fromList


removeColumn : Int -> Dict ( Int, Int ) String -> Dict ( Int, Int ) String
removeColumn colToRemove cells =
    cells
        |> Dict.toList
        |> List.filterMap
            (\( ( r, c ), v ) ->
                if c == colToRemove then
                    Nothing

                else if c > colToRemove then
                    Just ( ( r, c - 1 ), v )

                else
                    Just ( ( r, c ), v )
            )
        |> Dict.fromList


getCell : Int -> Int -> Dict ( Int, Int ) String -> String
getCell row col cells =
    Dict.get ( row, col ) cells |> Maybe.withDefault ""


removeColumnAlignments : Int -> Dict Int Alignment -> Dict Int Alignment
removeColumnAlignments colToRemove alignments =
    alignments
        |> Dict.toList
        |> List.filterMap
            (\( c, a ) ->
                if c == colToRemove then
                    Nothing

                else if c > colToRemove then
                    Just ( c - 1, a )

                else
                    Just ( c, a )
            )
        |> Dict.fromList


getHeaderAlignment : Int -> Dict Int Alignment -> Alignment
getHeaderAlignment col alignments =
    Dict.get col alignments |> Maybe.withDefault AlignCenter


getBodyAlignment : Int -> Dict Int Alignment -> Alignment
getBodyAlignment col alignments =
    Dict.get col alignments |> Maybe.withDefault AlignLeft


removeIndexFromDict : Int -> Dict Int a -> Dict Int a
removeIndexFromDict idx dict =
    dict
        |> Dict.toList
        |> List.filterMap
            (\( k, v ) ->
                if k == idx then
                    Nothing

                else if k > idx then
                    Just ( k - 1, v )

                else
                    Just ( k, v )
            )
        |> Dict.fromList


removeCellStyleRow : Int -> Dict ( Int, Int ) LineStyle -> Dict ( Int, Int ) LineStyle
removeCellStyleRow rowIdx dict =
    dict
        |> Dict.toList
        |> List.filterMap
            (\( ( h, c ), v ) ->
                if h == rowIdx || h == rowIdx + 1 then
                    Nothing

                else if h > rowIdx + 1 then
                    Just ( ( h - 1, c ), v )

                else
                    Just ( ( h, c ), v )
            )
        |> Dict.fromList


removeCellVStyleRow : Int -> Dict ( Int, Int ) LineStyle -> Dict ( Int, Int ) LineStyle
removeCellVStyleRow rowIdx dict =
    dict
        |> Dict.toList
        |> List.filterMap
            (\( ( r, vi ), v ) ->
                if r == rowIdx then
                    Nothing

                else if r > rowIdx then
                    Just ( ( r - 1, vi ), v )

                else
                    Just ( ( r, vi ), v )
            )
        |> Dict.fromList


removeCellStyleCol : Int -> Dict ( Int, Int ) LineStyle -> Dict ( Int, Int ) LineStyle
removeCellStyleCol colIdx dict =
    dict
        |> Dict.toList
        |> List.filterMap
            (\( ( h, c ), v ) ->
                if c == colIdx then
                    Nothing

                else if c > colIdx then
                    Just ( ( h, c - 1 ), v )

                else
                    Just ( ( h, c ), v )
            )
        |> Dict.fromList


removeCellVStyleCol : Int -> Dict ( Int, Int ) LineStyle -> Dict ( Int, Int ) LineStyle
removeCellVStyleCol colIdx dict =
    dict
        |> Dict.toList
        |> List.filterMap
            (\( ( r, vi ), v ) ->
                if vi == colIdx || vi == colIdx + 1 then
                    Nothing

                else if vi > colIdx + 1 then
                    Just ( ( r, vi - 1 ), v )

                else
                    Just ( ( r, vi ), v )
            )
        |> Dict.fromList


insertRow : Int -> Dict ( Int, Int ) String -> Dict ( Int, Int ) String
insertRow idx cells =
    cells
        |> Dict.toList
        |> List.map
            (\( ( r, c ), v ) ->
                if r >= idx then
                    ( ( r + 1, c ), v )

                else
                    ( ( r, c ), v )
            )
        |> Dict.fromList


insertColumn : Int -> Dict ( Int, Int ) String -> Dict ( Int, Int ) String
insertColumn idx cells =
    cells
        |> Dict.toList
        |> List.map
            (\( ( r, c ), v ) ->
                if c >= idx then
                    ( ( r, c + 1 ), v )

                else
                    ( ( r, c ), v )
            )
        |> Dict.fromList


insertColumnAlignments : Int -> Dict Int Alignment -> Dict Int Alignment
insertColumnAlignments idx alignments =
    alignments
        |> Dict.toList
        |> List.map
            (\( c, a ) ->
                if c >= idx then
                    ( c + 1, a )

                else
                    ( c, a )
            )
        |> Dict.fromList


insertIndexIntoDict : Int -> Dict Int a -> Dict Int a
insertIndexIntoDict idx dict =
    dict
        |> Dict.toList
        |> List.map
            (\( k, v ) ->
                if k >= idx then
                    ( k + 1, v )

                else
                    ( k, v )
            )
        |> Dict.fromList


insertCellStyleRow : Int -> Dict ( Int, Int ) LineStyle -> Dict ( Int, Int ) LineStyle
insertCellStyleRow rowIdx dict =
    dict
        |> Dict.toList
        |> List.map
            (\( ( h, c ), v ) ->
                if h > rowIdx then
                    ( ( h + 1, c ), v )

                else
                    ( ( h, c ), v )
            )
        |> Dict.fromList


insertCellVStyleRow : Int -> Dict ( Int, Int ) LineStyle -> Dict ( Int, Int ) LineStyle
insertCellVStyleRow rowIdx dict =
    dict
        |> Dict.toList
        |> List.map
            (\( ( r, vi ), v ) ->
                if r >= rowIdx then
                    ( ( r + 1, vi ), v )

                else
                    ( ( r, vi ), v )
            )
        |> Dict.fromList


insertCellStyleCol : Int -> Dict ( Int, Int ) LineStyle -> Dict ( Int, Int ) LineStyle
insertCellStyleCol colIdx dict =
    dict
        |> Dict.toList
        |> List.map
            (\( ( h, c ), v ) ->
                if c >= colIdx then
                    ( ( h, c + 1 ), v )

                else
                    ( ( h, c ), v )
            )
        |> Dict.fromList


insertCellVStyleCol : Int -> Dict ( Int, Int ) LineStyle -> Dict ( Int, Int ) LineStyle
insertCellVStyleCol colIdx dict =
    dict
        |> Dict.toList
        |> List.map
            (\( ( r, vi ), v ) ->
                if vi > colIdx then
                    ( ( r, vi + 1 ), v )

                else
                    ( ( r, vi ), v )
            )
        |> Dict.fromList


getHorizontalLineStyle : Int -> Dict Int LineStyle -> LineStyle
getHorizontalLineStyle idx styles =
    Dict.get idx styles |> Maybe.withDefault Thin


getVerticalLineStyle : Int -> Dict Int LineStyle -> LineStyle
getVerticalLineStyle idx styles =
    Dict.get idx styles |> Maybe.withDefault Thin


getEffectiveHStyle : Int -> Int -> Dict ( Int, Int ) LineStyle -> Dict Int LineStyle -> LineStyle
getEffectiveHStyle hIdx col cellStyles rowStyles =
    case Dict.get ( hIdx, col ) cellStyles of
        Just s ->
            s

        Nothing ->
            getHorizontalLineStyle hIdx rowStyles


getEffectiveVStyle : Int -> Int -> Dict ( Int, Int ) LineStyle -> Dict Int LineStyle -> LineStyle
getEffectiveVStyle row vIdx cellStyles colStyles =
    case Dict.get ( row, vIdx ) cellStyles of
        Just s ->
            s

        Nothing ->
            getVerticalLineStyle vIdx colStyles


cycleLineStyle : LineStyle -> LineStyle
cycleLineStyle style =
    case style of
        None ->
            Thin

        Thin ->
            Thick

        Thick ->
            ThinTripleDash

        ThinTripleDash ->
            ThickTripleDash

        ThickTripleDash ->
            ThinQuadDash

        ThinQuadDash ->
            ThickQuadDash

        ThickQuadDash ->
            ThinDoubleDash

        ThinDoubleDash ->
            ThickDoubleDash

        ThickDoubleDash ->
            Double

        Double ->
            None


lineStyleWeight : LineStyle -> LineWeight
lineStyleWeight style =
    case style of
        None ->
            WNone

        Thin ->
            WLight

        Thick ->
            WHeavy

        ThinTripleDash ->
            WLight

        ThickTripleDash ->
            WHeavy

        ThinQuadDash ->
            WLight

        ThickQuadDash ->
            WHeavy

        ThinDoubleDash ->
            WLight

        ThickDoubleDash ->
            WHeavy

        Double ->
            WDouble


horizontalChar : LineStyle -> String
horizontalChar style =
    case style of
        None ->
            " "

        Thin ->
            "─"

        Thick ->
            "━"

        ThinTripleDash ->
            "┄"

        ThickTripleDash ->
            "┅"

        ThinQuadDash ->
            "┈"

        ThickQuadDash ->
            "┉"

        ThinDoubleDash ->
            "╌"

        ThickDoubleDash ->
            "╍"

        Double ->
            "═"


verticalChar : LineStyle -> String
verticalChar style =
    case style of
        None ->
            " "

        Thin ->
            "│"

        Thick ->
            "┃"

        ThinTripleDash ->
            "┆"

        ThickTripleDash ->
            "┇"

        ThinQuadDash ->
            "┊"

        ThickQuadDash ->
            "┋"

        ThinDoubleDash ->
            "╎"

        ThickDoubleDash ->
            "╏"

        Double ->
            "║"


weightCode : LineWeight -> Int
weightCode w =
    case w of
        WNone ->
            0

        WLight ->
            1

        WHeavy ->
            2

        WDouble ->
            3


cornerKey : LineWeight -> LineWeight -> LineWeight -> LineWeight -> Int
cornerKey up down left right =
    weightCode up * 64 + weightCode down * 16 + weightCode left * 4 + weightCode right


lookupCorner : LineWeight -> LineWeight -> LineWeight -> LineWeight -> String
lookupCorner up down left right =
    if up == WNone && down == WNone && left == WNone && right == WNone then
        " "

    else
        let
            key =
                cornerKey up down left right
        in
        case Dict.get key cornerDict of
            Just ch ->
                ch

            Nothing ->
                -- Fallback 1: downgrade Heavy to Light when mixing with Double
                let
                    fix w =
                        if w == WHeavy then
                            WLight

                        else
                            w

                    fallbackKey =
                        cornerKey (fix up) (fix down) (fix left) (fix right)
                in
                case Dict.get fallbackKey cornerDict of
                    Just ch ->
                        ch

                    Nothing ->
                        -- Fallback 2: downgrade both Heavy and Double to Light
                        let
                            simplify w =
                                case w of
                                    WHeavy ->
                                        WLight

                                    WDouble ->
                                        WLight

                                    _ ->
                                        w

                            simpleKey =
                                cornerKey (simplify up) (simplify down) (simplify left) (simplify right)
                        in
                        Dict.get simpleKey cornerDict |> Maybe.withDefault " "


cornerDict : Dict Int String
cornerDict =
    Dict.fromList
        [ -- All Light
          ( cornerKey WNone WLight WNone WLight, "┌" )
        , ( cornerKey WNone WLight WLight WLight, "┬" )
        , ( cornerKey WNone WLight WLight WNone, "┐" )
        , ( cornerKey WLight WLight WNone WLight, "├" )
        , ( cornerKey WLight WLight WLight WLight, "┼" )
        , ( cornerKey WLight WLight WLight WNone, "┤" )
        , ( cornerKey WLight WNone WNone WLight, "└" )
        , ( cornerKey WLight WNone WLight WLight, "┴" )
        , ( cornerKey WLight WNone WLight WNone, "┘" )

        -- All Heavy
        , ( cornerKey WNone WHeavy WNone WHeavy, "┏" )
        , ( cornerKey WNone WHeavy WHeavy WHeavy, "┳" )
        , ( cornerKey WNone WHeavy WHeavy WNone, "┓" )
        , ( cornerKey WHeavy WHeavy WNone WHeavy, "┣" )
        , ( cornerKey WHeavy WHeavy WHeavy WHeavy, "╋" )
        , ( cornerKey WHeavy WHeavy WHeavy WNone, "┫" )
        , ( cornerKey WHeavy WNone WNone WHeavy, "┗" )
        , ( cornerKey WHeavy WNone WHeavy WHeavy, "┻" )
        , ( cornerKey WHeavy WNone WHeavy WNone, "┛" )

        -- All Double
        , ( cornerKey WNone WDouble WNone WDouble, "╔" )
        , ( cornerKey WNone WDouble WDouble WDouble, "╦" )
        , ( cornerKey WNone WDouble WDouble WNone, "╗" )
        , ( cornerKey WDouble WDouble WNone WDouble, "╠" )
        , ( cornerKey WDouble WDouble WDouble WDouble, "╬" )
        , ( cornerKey WDouble WDouble WDouble WNone, "╣" )
        , ( cornerKey WDouble WNone WNone WDouble, "╚" )
        , ( cornerKey WDouble WNone WDouble WDouble, "╩" )
        , ( cornerKey WDouble WNone WDouble WNone, "╝" )

        -- Light vertical, Heavy horizontal
        , ( cornerKey WNone WLight WNone WHeavy, "┍" )
        , ( cornerKey WNone WLight WHeavy WHeavy, "┯" )
        , ( cornerKey WNone WLight WHeavy WNone, "┑" )
        , ( cornerKey WLight WLight WNone WHeavy, "┝" )
        , ( cornerKey WLight WLight WHeavy WHeavy, "┿" )
        , ( cornerKey WLight WLight WHeavy WNone, "┥" )
        , ( cornerKey WLight WNone WNone WHeavy, "┕" )
        , ( cornerKey WLight WNone WHeavy WHeavy, "┷" )
        , ( cornerKey WLight WNone WHeavy WNone, "┙" )

        -- Heavy vertical, Light horizontal
        , ( cornerKey WNone WHeavy WNone WLight, "┎" )
        , ( cornerKey WNone WHeavy WLight WLight, "┰" )
        , ( cornerKey WNone WHeavy WLight WNone, "┒" )
        , ( cornerKey WHeavy WHeavy WNone WLight, "┠" )
        , ( cornerKey WHeavy WHeavy WLight WLight, "╂" )
        , ( cornerKey WHeavy WHeavy WLight WNone, "┨" )
        , ( cornerKey WHeavy WNone WNone WLight, "┖" )
        , ( cornerKey WHeavy WNone WLight WLight, "┸" )
        , ( cornerKey WHeavy WNone WLight WNone, "┚" )

        -- Light vertical, Double horizontal
        , ( cornerKey WNone WLight WNone WDouble, "╒" )
        , ( cornerKey WNone WLight WDouble WDouble, "╤" )
        , ( cornerKey WNone WLight WDouble WNone, "╕" )
        , ( cornerKey WLight WLight WNone WDouble, "╞" )
        , ( cornerKey WLight WLight WDouble WDouble, "╪" )
        , ( cornerKey WLight WLight WDouble WNone, "╡" )
        , ( cornerKey WLight WNone WNone WDouble, "╘" )
        , ( cornerKey WLight WNone WDouble WDouble, "╧" )
        , ( cornerKey WLight WNone WDouble WNone, "╛" )

        -- Double vertical, Light horizontal
        , ( cornerKey WNone WDouble WNone WLight, "╓" )
        , ( cornerKey WNone WDouble WLight WLight, "╥" )
        , ( cornerKey WNone WDouble WLight WNone, "╖" )
        , ( cornerKey WDouble WDouble WNone WLight, "╟" )
        , ( cornerKey WDouble WDouble WLight WLight, "╫" )
        , ( cornerKey WDouble WDouble WLight WNone, "╢" )
        , ( cornerKey WDouble WNone WNone WLight, "╙" )
        , ( cornerKey WDouble WNone WLight WLight, "╨" )
        , ( cornerKey WDouble WNone WLight WNone, "╜" )

        -- Mixed: Light down, Heavy up (and vice versa) with Light horizontal
        , ( cornerKey WLight WHeavy WNone WLight, "┟" )
        , ( cornerKey WLight WHeavy WLight WLight, "╁" )
        , ( cornerKey WLight WHeavy WLight WNone, "┧" )
        , ( cornerKey WHeavy WLight WNone WLight, "┞" )
        , ( cornerKey WHeavy WLight WLight WLight, "╀" )
        , ( cornerKey WHeavy WLight WLight WNone, "┦" )

        -- Mixed: Light down, Heavy up with Heavy horizontal
        , ( cornerKey WLight WHeavy WNone WHeavy, "┢" )
        , ( cornerKey WLight WHeavy WHeavy WHeavy, "╊" )
        , ( cornerKey WLight WHeavy WHeavy WNone, "┪" )
        , ( cornerKey WHeavy WLight WNone WHeavy, "┡" )
        , ( cornerKey WHeavy WLight WHeavy WHeavy, "╉" )
        , ( cornerKey WHeavy WLight WHeavy WNone, "┩" )

        -- Mixed horizontal: Light left, Heavy right (and vice versa) with Light vertical
        , ( cornerKey WNone WLight WLight WHeavy, "┭" )
        , ( cornerKey WNone WLight WHeavy WLight, "┮" )
        , ( cornerKey WLight WLight WLight WHeavy, "┽" )
        , ( cornerKey WLight WLight WHeavy WLight, "┾" )
        , ( cornerKey WLight WNone WLight WHeavy, "┵" )
        , ( cornerKey WLight WNone WHeavy WLight, "┶" )

        -- Mixed horizontal: Light left, Heavy right with Heavy vertical
        , ( cornerKey WNone WHeavy WLight WHeavy, "┱" )
        , ( cornerKey WNone WHeavy WHeavy WLight, "┲" )
        , ( cornerKey WHeavy WHeavy WLight WHeavy, "╅" )
        , ( cornerKey WHeavy WHeavy WHeavy WLight, "╆" )
        , ( cornerKey WHeavy WNone WLight WHeavy, "┹" )
        , ( cornerKey WHeavy WNone WHeavy WLight, "┺" )

        -- All four different: vertical mixed + horizontal mixed
        , ( cornerKey WLight WHeavy WLight WHeavy, "╃" )
        , ( cornerKey WLight WHeavy WHeavy WLight, "╄" )
        , ( cornerKey WHeavy WLight WLight WHeavy, "╇" )
        , ( cornerKey WHeavy WLight WHeavy WLight, "╈" )
        ]


lineStyleLabel : LineStyle -> String
lineStyleLabel style =
    case style of
        None ->
            "None"

        Thin ->
            "Thin"

        Thick ->
            "Thick"

        ThinTripleDash ->
            "Triple Dash"

        ThickTripleDash ->
            "Thick Triple"

        ThinQuadDash ->
            "Quad Dash"

        ThickQuadDash ->
            "Thick Quad"

        ThinDoubleDash ->
            "Double Dash"

        ThickDoubleDash ->
            "Thick Dbl Dash"

        Double ->
            "Double"


hSepLabel : Int -> Int -> String
hSepLabel idx rows =
    if idx == 0 then
        "Top border"

    else if idx == rows then
        "Bottom border"

    else
        "Row " ++ String.fromInt idx ++ "-" ++ String.fromInt (idx + 1)


vSepLabel : Int -> Int -> String
vSepLabel idx cols =
    if idx == 0 then
        "Left border"

    else if idx == cols then
        "Right border"

    else
        "Col " ++ String.fromInt idx ++ "-" ++ String.fromInt (idx + 1)


lineStyleToCss : LineStyle -> String
lineStyleToCss style =
    case style of
        None ->
            "none"

        Thin ->
            "1px solid"

        Thick ->
            "3px solid"

        ThinTripleDash ->
            "1px dashed"

        ThickTripleDash ->
            "3px dashed"

        ThinQuadDash ->
            "1px dotted"

        ThickQuadDash ->
            "3px dotted"

        ThinDoubleDash ->
            "1px dashed"

        ThickDoubleDash ->
            "3px dashed"

        Double ->
            "3px double"


padLeft : String -> Int -> String
padLeft content width =
    String.repeat (width - String.length content) " " ++ content


padCenter : String -> Int -> String
padCenter content width =
    let
        totalPad =
            width - String.length content

        lp =
            totalPad // 2

        rp =
            totalPad - lp
    in
    String.repeat lp " " ++ content ++ String.repeat rp " "


padContent : Alignment -> String -> Int -> String
padContent align content width =
    case align of
        AlignLeft ->
            content ++ String.repeat (width - String.length content) " "

        AlignCenter ->
            padCenter content width

        AlignRight ->
            padLeft content width


escapePipe : String -> String
escapePipe s =
    String.replace "|" "\\|" s



-- IMPORT PARSING


parseImportData : String -> { rows : Int, cols : Int, cells : Dict ( Int, Int ) String }
parseImportData input =
    let
        trimmed =
            String.trim input

        lines =
            if String.isEmpty trimmed then
                []

            else
                String.split "\n" trimmed

        delimiter =
            if List.any (String.contains "\t") lines then
                "\t"

            else
                ","

        parsedRows =
            List.map (String.split delimiter >> List.map String.trim) lines

        numRows =
            List.length parsedRows

        numCols =
            List.foldl (\row maxC -> max maxC (List.length row)) 0 parsedRows

        cells =
            parsedRows
                |> List.indexedMap
                    (\r row ->
                        List.indexedMap (\c val -> ( ( r, c ), val )) row
                    )
                |> List.concat
                |> Dict.fromList
    in
    { rows = numRows, cols = numCols, cells = cells }



-- MARKDOWN GENERATION


generateMarkdown : OutputFormat -> Int -> Int -> Dict ( Int, Int ) String -> Dict Int Alignment -> Dict Int Alignment -> List Int -> List SummaryFunction -> String
generateMarkdown format rows cols cells headerAlignments bodyAlignments bodyRowOrder summaryFunctions =
    if rows == 0 || cols == 0 then
        ""

    else
        let
            colRange =
                List.range 0 (cols - 1)

            rowRange =
                List.range 0 (rows - 1)

            bodyRowIndicesForSummary =
                List.range 1 (rows - 1)

            summaryTexts =
                List.map
                    (\fn -> computeSummaryRow fn cols cells bodyRowIndicesForSummary |> List.map (\v -> "**" ++ v ++ "**"))
                    summaryFunctions

            colWidths =
                List.map
                    (\c ->
                        let
                            cellMax =
                                List.foldl
                                    (\r maxW ->
                                        max maxW
                                            (String.length (escapePipe (getCell r c cells)))
                                    )
                                    3
                                    rowRange

                            summaryMax =
                                List.foldl
                                    (\row maxW ->
                                        case List.head (List.drop c row) of
                                            Just v ->
                                                max maxW (String.length v)

                                            Nothing ->
                                                maxW
                                    )
                                    0
                                    summaryTexts
                        in
                        max cellMax summaryMax
                    )
                    colRange

            formatRow r =
                let
                    rowAlignments =
                        if r == 0 then
                            headerAlignments

                        else
                            bodyAlignments

                    getRowAlignment =
                        if r == 0 then
                            getHeaderAlignment

                        else
                            getBodyAlignment

                    cellTexts =
                        case format of
                            Compact ->
                                List.map
                                    (\c -> escapePipe (getCell r c cells))
                                    colRange

                            Expanded ->
                                List.map2
                                    (\c w ->
                                        padContent (getRowAlignment c rowAlignments)
                                            (escapePipe (getCell r c cells))
                                            w
                                    )
                                    colRange
                                    colWidths
                in
                "| " ++ String.join " | " cellTexts ++ " |"

            separatorCell align w =
                case format of
                    Compact ->
                        case align of
                            AlignLeft ->
                                "---"

                            AlignCenter ->
                                ":-:"

                            AlignRight ->
                                "--:"

                    Expanded ->
                        case align of
                            AlignLeft ->
                                String.repeat w "-"

                            AlignCenter ->
                                ":" ++ String.repeat (w - 2) "-" ++ ":"

                            AlignRight ->
                                String.repeat (w - 1) "-" ++ ":"

            separatorRow =
                "| "
                    ++ (List.map2
                            (\c w -> separatorCell (getBodyAlignment c bodyAlignments) w)
                            colRange
                            colWidths
                            |> String.join " | "
                       )
                    ++ " |"

            headerRow =
                formatRow 0

            dataRows =
                List.map formatRow bodyRowOrder

            summaryRowLines =
                List.map
                    (\boldVals ->
                        let
                            cellTexts =
                                case format of
                                    Compact ->
                                        boldVals

                                    Expanded ->
                                        List.map2
                                            (\v w -> padContent AlignLeft v w)
                                            boldVals
                                            colWidths
                        in
                        "| " ++ String.join " | " cellTexts ++ " |"
                    )
                    summaryTexts
        in
        String.join "\n" (headerRow :: separatorRow :: dataRows ++ summaryRowLines)



-- BOX DRAWING GENERATION


generateBoxDrawing : Int -> Int -> Dict ( Int, Int ) String -> Dict Int Alignment -> Dict Int Alignment -> Dict Int LineStyle -> Dict Int LineStyle -> Dict ( Int, Int ) LineStyle -> Dict ( Int, Int ) LineStyle -> List Int -> List SummaryFunction -> Dict Int LineStyle -> String
generateBoxDrawing rows cols cells headerAlignments bodyAlignments hStyles vStyles cellHStyles cellVStyles bodyRowOrder summaryFunctions summarySepStyles =
    if rows == 0 || cols == 0 then
        ""

    else
        let
            colRange =
                List.range 0 (cols - 1)

            rowRange =
                List.range 0 (rows - 1)

            bodyRowIndicesForSummary =
                List.range 1 (rows - 1)

            summaryRowValues =
                List.map
                    (\fn -> computeSummaryRow fn cols cells bodyRowIndicesForSummary)
                    summaryFunctions

            colWidths =
                List.map
                    (\c ->
                        let
                            cellMax =
                                List.foldl
                                    (\r maxW -> max maxW (String.length (getCell r c cells)))
                                    1
                                    rowRange

                            summaryMax =
                                List.foldl
                                    (\row maxW ->
                                        case List.head (List.drop c row) of
                                            Just v ->
                                                max maxW (String.length v)

                                            Nothing ->
                                                maxW
                                    )
                                    0
                                    summaryRowValues
                        in
                        max cellMax summaryMax
                    )
                    colRange

            effH hIdx col =
                getEffectiveHStyle hIdx col cellHStyles hStyles

            effV row vIdx =
                getEffectiveVStyle row vIdx cellVStyles vStyles

            horizontalLine hIdx =
                let
                    segments =
                        List.map2
                            (\c w -> String.repeat (w + 2) (horizontalChar (effH hIdx c)))
                            colRange
                            colWidths

                    intersections =
                        List.map
                            (\vIdx ->
                                let
                                    up =
                                        if hIdx > 0 then
                                            lineStyleWeight (effV (hIdx - 1) vIdx)

                                        else
                                            WNone

                                    down =
                                        if hIdx < rows then
                                            lineStyleWeight (effV hIdx vIdx)

                                        else
                                            WNone

                                    left =
                                        lineStyleWeight (effH hIdx (vIdx - 1))

                                    right =
                                        lineStyleWeight (effH hIdx vIdx)
                                in
                                lookupCorner up down left right
                            )
                            (List.range 1 (cols - 1))

                    leftCorner =
                        let
                            up =
                                if hIdx > 0 then
                                    lineStyleWeight (effV (hIdx - 1) 0)

                                else
                                    WNone

                            down =
                                if hIdx < rows then
                                    lineStyleWeight (effV hIdx 0)

                                else
                                    WNone
                        in
                        lookupCorner up down WNone (lineStyleWeight (effH hIdx 0))

                    rightCorner =
                        let
                            up =
                                if hIdx > 0 then
                                    lineStyleWeight (effV (hIdx - 1) cols)

                                else
                                    WNone

                            down =
                                if hIdx < rows then
                                    lineStyleWeight (effV hIdx cols)

                                else
                                    WNone
                        in
                        lookupCorner up down (lineStyleWeight (effH hIdx (cols - 1))) WNone
                in
                leftCorner ++ String.concat (interleave segments intersections) ++ rightCorner

            formatRow r =
                let
                    leftV =
                        verticalChar (effV r 0)

                    rightV =
                        verticalChar (effV r cols)

                    getRowAlignment =
                        if r == 0 then
                            getHeaderAlignment

                        else
                            getBodyAlignment

                    rowAlignments =
                        if r == 0 then
                            headerAlignments

                        else
                            bodyAlignments

                    cellTexts =
                        List.map2
                            (\c w ->
                                padContent (getRowAlignment c rowAlignments)
                                    (getCell r c cells)
                                    w
                            )
                            colRange
                            colWidths

                    innerSeps =
                        List.map
                            (\vIdx -> verticalChar (effV r vIdx))
                            (List.range 1 (cols - 1))
                in
                leftV ++ " " ++ String.concat (interleave cellTexts (List.map (\s -> " " ++ s ++ " ") innerSeps)) ++ " " ++ rightV

            isSorted =
                bodyRowOrder /= List.range 1 (rows - 1)

            defaultHLine =
                let
                    segments =
                        List.map2
                            (\_ w -> String.repeat (w + 2) (horizontalChar Thin))
                            colRange
                            colWidths

                    innerIntersections =
                        List.map
                            (\_ -> lookupCorner WLight WLight WLight WLight)
                            (List.range 1 (cols - 1))

                    leftC =
                        lookupCorner WLight WLight WNone WLight

                    rightC =
                        lookupCorner WLight WLight WLight WNone
                in
                leftC ++ String.concat (interleave segments innerIntersections) ++ rightC

            bodyLen =
                List.length bodyRowOrder

            formatSummaryRowBox vals =
                let
                    leftV =
                        verticalChar Thin

                    rightV =
                        verticalChar Thin

                    cellTexts =
                        List.map2
                            (\v w -> padContent AlignLeft v w)
                            vals
                            colWidths

                    innerSeps =
                        List.map (\_ -> verticalChar Thin) (List.range 1 (cols - 1))
                in
                leftV ++ " " ++ String.concat (interleave cellTexts (List.map (\s -> " " ++ s ++ " ") innerSeps)) ++ " " ++ rightV

            summarySepLine sepIdx =
                let
                    style =
                        Dict.get sepIdx summarySepStyles |> Maybe.withDefault Thin

                    segments =
                        List.map2
                            (\_ w -> String.repeat (w + 2) (horizontalChar style))
                            colRange
                            colWidths

                    innerIntersections =
                        List.map
                            (\_ -> lookupCorner WLight WLight (lineStyleWeight style) (lineStyleWeight style))
                            (List.range 1 (cols - 1))

                    leftC =
                        lookupCorner WLight WLight WNone (lineStyleWeight style)

                    rightC =
                        lookupCorner WLight WLight (lineStyleWeight style) WNone
                in
                leftC ++ String.concat (interleave segments innerIntersections) ++ rightC

            summarySection =
                case summaryRowValues of
                    [] ->
                        []

                    first :: rest ->
                        horizontalLine rows
                            :: formatSummaryRowBox first
                            :: List.concat (List.indexedMap (\i vals -> [ summarySepLine i, formatSummaryRowBox vals ]) rest)

            allRows =
                [ horizontalLine 0, formatRow 0 ]
                    ++ (if bodyLen > 0 then
                            horizontalLine 1
                                :: List.concatMap
                                    (\( i, r ) ->
                                        if i < bodyLen - 1 then
                                            [ formatRow r
                                            , if isSorted then
                                                defaultHLine

                                              else
                                                horizontalLine (r + 1)
                                            ]

                                        else
                                            [ formatRow r ]
                                    )
                                    (List.indexedMap Tuple.pair bodyRowOrder)

                        else
                            []
                       )
                    ++ summarySection
                    ++ [ horizontalLine rows ]
        in
        String.join "\n" allRows


interleave : List String -> List String -> List String
interleave a b =
    case ( a, b ) of
        ( [], _ ) ->
            []

        ( x :: _, [] ) ->
            [ x ]

        ( x :: xs, y :: ys ) ->
            x :: y :: interleave xs ys



-- HTML TABLE GENERATION


alignmentToStyle : Alignment -> String
alignmentToStyle align =
    case align of
        AlignLeft ->
            "left"

        AlignCenter ->
            "center"

        AlignRight ->
            "right"


generateHtmlTable : Int -> Int -> Dict ( Int, Int ) String -> Dict Int Alignment -> Dict Int Alignment -> Dict Int LineStyle -> Dict Int LineStyle -> Dict ( Int, Int ) LineStyle -> Dict ( Int, Int ) LineStyle -> List Int -> List SummaryFunction -> String
generateHtmlTable rows cols cells headerAlignments bodyAlignments hStyles vStyles cellHStyles cellVStyles bodyRowOrder summaryFunctions =
    if rows == 0 || cols == 0 then
        ""

    else
        let
            colRange =
                List.range 0 (cols - 1)

            indent n =
                String.repeat n "  "

            escapeHtml s =
                s
                    |> String.replace "&" "&amp;"
                    |> String.replace "<" "&lt;"
                    |> String.replace ">" "&gt;"
                    |> String.replace "\"" "&quot;"

            cellBorderStyle r c =
                let
                    top =
                        getEffectiveHStyle r c cellHStyles hStyles

                    bottom =
                        getEffectiveHStyle (r + 1) c cellHStyles hStyles

                    left =
                        getEffectiveVStyle r c cellVStyles vStyles

                    right =
                        getEffectiveVStyle r (c + 1) cellVStyles vStyles

                    parts =
                        List.filterMap identity
                            [ if top /= Thin then
                                Just ("border-top: " ++ lineStyleToCss top)

                              else
                                Nothing
                            , if bottom /= Thin then
                                Just ("border-bottom: " ++ lineStyleToCss bottom)

                              else
                                Nothing
                            , if left /= Thin then
                                Just ("border-left: " ++ lineStyleToCss left)

                              else
                                Nothing
                            , if right /= Thin then
                                Just ("border-right: " ++ lineStyleToCss right)

                              else
                                Nothing
                            ]
                in
                parts

            cellStyleAttr r c align =
                let
                    alignParts =
                        if align /= AlignLeft then
                            [ "text-align: " ++ alignmentToStyle align ]

                        else
                            []

                    borderParts =
                        cellBorderStyle r c

                    allParts =
                        alignParts ++ borderParts
                in
                if List.isEmpty allParts then
                    ""

                else
                    " style=\"" ++ String.join "; " allParts ++ "\""

            headerCells =
                List.map
                    (\c ->
                        let
                            align =
                                getHeaderAlignment c headerAlignments
                        in
                        indent 3 ++ "<th" ++ cellStyleAttr 0 c align ++ ">" ++ escapeHtml (getCell 0 c cells) ++ "</th>"
                    )
                    colRange

            headerSection =
                [ indent 1 ++ "<thead>"
                , indent 2 ++ "<tr>"
                ]
                    ++ headerCells
                    ++ [ indent 2 ++ "</tr>"
                       , indent 1 ++ "</thead>"
                       ]

            bodyRow r =
                let
                    rowCells =
                        List.map
                            (\c ->
                                let
                                    align =
                                        getBodyAlignment c bodyAlignments
                                in
                                indent 3 ++ "<td" ++ cellStyleAttr r c align ++ ">" ++ escapeHtml (getCell r c cells) ++ "</td>"
                            )
                            colRange
                in
                (indent 2 ++ "<tr>") :: rowCells ++ [ indent 2 ++ "</tr>" ]

            bodyRows =
                List.concatMap bodyRow bodyRowOrder

            bodySection =
                if rows > 1 then
                    (indent 1 ++ "<tbody>") :: bodyRows ++ [ indent 1 ++ "</tbody>" ]

                else
                    []

            bodyRowIndicesForSummary =
                List.range 1 (rows - 1)

            footSection =
                if List.isEmpty summaryFunctions then
                    []

                else
                    let
                        footRows =
                            List.concatMap
                                (\fn ->
                                    let
                                        vals =
                                            computeSummaryRow fn cols cells bodyRowIndicesForSummary

                                        footCells =
                                            List.indexedMap
                                                (\c v ->
                                                    if c == 0 then
                                                        indent 3 ++ "<th scope=\"row\">" ++ escapeHtml v ++ "</th>"

                                                    else
                                                        indent 3 ++ "<td>" ++ escapeHtml v ++ "</td>"
                                                )
                                                vals
                                    in
                                    (indent 2 ++ "<tr>") :: footCells ++ [ indent 2 ++ "</tr>" ]
                                )
                                summaryFunctions
                    in
                    (indent 1 ++ "<tfoot>") :: footRows ++ [ indent 1 ++ "</tfoot>" ]
        in
        String.join "\n" ("<table>" :: headerSection ++ bodySection ++ footSection ++ [ "</table>" ])



-- ICONS


alignLeftIcon : Html msg
alignLeftIcon =
    Svg.svg
        [ SvgAttr.width "14"
        , SvgAttr.height "14"
        , SvgAttr.viewBox "0 0 14 14"
        ]
        [ Svg.line [ SvgAttr.x1 "1", SvgAttr.y1 "3", SvgAttr.x2 "13", SvgAttr.y2 "3", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5", SvgAttr.strokeLinecap "round" ] []
        , Svg.line [ SvgAttr.x1 "1", SvgAttr.y1 "7", SvgAttr.x2 "9", SvgAttr.y2 "7", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5", SvgAttr.strokeLinecap "round" ] []
        , Svg.line [ SvgAttr.x1 "1", SvgAttr.y1 "11", SvgAttr.x2 "11", SvgAttr.y2 "11", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5", SvgAttr.strokeLinecap "round" ] []
        ]


alignCenterIcon : Html msg
alignCenterIcon =
    Svg.svg
        [ SvgAttr.width "14"
        , SvgAttr.height "14"
        , SvgAttr.viewBox "0 0 14 14"
        ]
        [ Svg.line [ SvgAttr.x1 "1", SvgAttr.y1 "3", SvgAttr.x2 "13", SvgAttr.y2 "3", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5", SvgAttr.strokeLinecap "round" ] []
        , Svg.line [ SvgAttr.x1 "3", SvgAttr.y1 "7", SvgAttr.x2 "11", SvgAttr.y2 "7", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5", SvgAttr.strokeLinecap "round" ] []
        , Svg.line [ SvgAttr.x1 "2", SvgAttr.y1 "11", SvgAttr.x2 "12", SvgAttr.y2 "11", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5", SvgAttr.strokeLinecap "round" ] []
        ]


alignRightIcon : Html msg
alignRightIcon =
    Svg.svg
        [ SvgAttr.width "14"
        , SvgAttr.height "14"
        , SvgAttr.viewBox "0 0 14 14"
        ]
        [ Svg.line [ SvgAttr.x1 "1", SvgAttr.y1 "3", SvgAttr.x2 "13", SvgAttr.y2 "3", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5", SvgAttr.strokeLinecap "round" ] []
        , Svg.line [ SvgAttr.x1 "5", SvgAttr.y1 "7", SvgAttr.x2 "13", SvgAttr.y2 "7", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5", SvgAttr.strokeLinecap "round" ] []
        , Svg.line [ SvgAttr.x1 "3", SvgAttr.y1 "11", SvgAttr.x2 "13", SvgAttr.y2 "11", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5", SvgAttr.strokeLinecap "round" ] []
        ]


sortAscIcon : Html msg
sortAscIcon =
    Svg.svg
        [ SvgAttr.width "14"
        , SvgAttr.height "14"
        , SvgAttr.viewBox "0 0 14 14"
        , SvgAttr.fill "none"
        ]
        [ Svg.line [ SvgAttr.x1 "2", SvgAttr.y1 "3", SvgAttr.x2 "12", SvgAttr.y2 "3", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5", SvgAttr.strokeLinecap "round" ] []
        , Svg.line [ SvgAttr.x1 "2", SvgAttr.y1 "7", SvgAttr.x2 "9", SvgAttr.y2 "7", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5", SvgAttr.strokeLinecap "round" ] []
        , Svg.line [ SvgAttr.x1 "2", SvgAttr.y1 "11", SvgAttr.x2 "6", SvgAttr.y2 "11", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5", SvgAttr.strokeLinecap "round" ] []
        ]


sortDescIcon : Html msg
sortDescIcon =
    Svg.svg
        [ SvgAttr.width "14"
        , SvgAttr.height "14"
        , SvgAttr.viewBox "0 0 14 14"
        , SvgAttr.fill "none"
        ]
        [ Svg.line [ SvgAttr.x1 "2", SvgAttr.y1 "3", SvgAttr.x2 "6", SvgAttr.y2 "3", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5", SvgAttr.strokeLinecap "round" ] []
        , Svg.line [ SvgAttr.x1 "2", SvgAttr.y1 "7", SvgAttr.x2 "9", SvgAttr.y2 "7", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5", SvgAttr.strokeLinecap "round" ] []
        , Svg.line [ SvgAttr.x1 "2", SvgAttr.y1 "11", SvgAttr.x2 "12", SvgAttr.y2 "11", SvgAttr.stroke "currentColor", SvgAttr.strokeWidth "1.5", SvgAttr.strokeLinecap "round" ] []
        ]



-- VIEW


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Tabular"
    , body =
        [ globalStyles
        , div [ Attr.class "app" ]
            [ viewHeader
            , viewTableEditor model
            , viewBoxDrawingOutput model
            , viewMarkdownOutput model
            , viewRenderedTable model
            , viewHtmlTableOutput model
            ]
        ]
    }


globalStyles : Html msg
globalStyles =
    node "style"
        []
        [ text """
* { margin: 0; padding: 0; box-sizing: border-box; }

body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
    background: #f0f2f5;
    color: #1a1a2e;
    min-height: 100vh;
}

.app {
    max-width: 960px;
    margin: 0 auto;
    padding: 2rem 1rem;
}

.header {
    text-align: center;
    margin-bottom: 2rem;
}

.header h1 {
    font-size: 2rem;
    font-weight: 700;
    color: #1a1a2e;
    letter-spacing: -0.02em;
}

.header p {
    color: #6b7280;
    margin-top: 0.25rem;
    font-size: 0.95rem;
}

.table-container {
    background: white;
    border-radius: 12px;
    padding: 1.5rem;
    box-shadow: 0 1px 3px rgba(0,0,0,0.08), 0 1px 2px rgba(0,0,0,0.06);
    margin-bottom: 1rem;
    overflow-x: auto;
}

.editor-table {
    border-collapse: collapse;
}

.editor-table td,
.editor-table th {
    padding: 2px;
}

.cell-input {
    width: 140px;
    padding: 8px 10px;
    border: 1px solid #e5e7eb;
    border-radius: 6px;
    font-size: 14px;
    font-family: inherit;
    transition: border-color 0.15s, box-shadow 0.15s;
    outline: none;
    background: #fafafa;
}

.cell-input:focus {
    border-color: #4a90d9;
    box-shadow: 0 0 0 3px rgba(74, 144, 217, 0.12);
    background: white;
}

.cell-input.header-cell {
    font-weight: 600;
    background: #f0f4ff;
    border-color: #d0d9e8;
}

.cell-input.header-cell:focus {
    background: white;
    border-color: #4a90d9;
}

.btn-pill {
    display: inline-flex;
    border: 1px solid #e5e7eb;
    border-radius: 6px;
    overflow: hidden;
}

.pill-add-btn, .pill-del-btn {
    padding: 4px 6px;
    border: none;
    background: white;
    cursor: pointer;
    font-size: 14px;
    font-family: inherit;
    font-weight: 600;
    transition: all 0.15s;
    line-height: 1;
    color: #9ca3af;
}

.pill-add-btn {
    border-right: 1px solid #e5e7eb;
}

.pill-add-btn:hover {
    color: #22c55e;
    background: #f0fdf4;
}

.pill-del-btn:hover {
    color: #ef4444;
    background: #fef2f2;
}

.pill-del-btn:disabled {
    opacity: 0.25;
    cursor: not-allowed;
}

.pill-del-btn:disabled:hover {
    color: #9ca3af;
    background: white;
}

.align-group {
    display: inline-flex;
    border: 1px solid #e5e7eb;
    border-radius: 6px;
    overflow: hidden;
}

.align-btn {
    padding: 4px 6px;
    border: none;
    border-right: 1px solid #e5e7eb;
    background: white;
    color: #9ca3af;
    cursor: pointer;
    font-size: 11px;
    font-family: inherit;
    font-weight: 600;
    transition: all 0.15s;
    line-height: 1.2;
}

.align-btn svg {
    display: block;
}

.align-btn:last-child {
    border-right: none;
}

.align-btn:hover {
    background: #f0f4ff;
    color: #4a90d9;
}

.align-btn.active {
    background: #4a90d9;
    color: white;
}

.add-btn {
    padding: 6px 14px;
    background: #f8fafc;
    border: 1px dashed #cbd5e1;
    border-radius: 8px;
    color: #64748b;
    cursor: pointer;
    font-size: 13px;
    font-family: inherit;
    transition: all 0.15s;
    display: inline-flex;
    align-items: center;
    gap: 4px;
}

.add-btn:hover:not(:disabled) {
    background: #f0f4ff;
    border-color: #4a90d9;
    color: #4a90d9;
}

.add-btn:disabled {
    opacity: 0.4;
    cursor: not-allowed;
}

.button-row {
    display: flex;
    gap: 8px;
    margin-top: 12px;
}

.output-controls {
    display: flex;
    align-items: center;
    gap: 6px;
}

.format-btn {
    padding: 6px 16px;
    border: 1px solid #e5e7eb;
    border-radius: 8px;
    background: white;
    color: #6b7280;
    cursor: pointer;
    font-size: 13px;
    font-family: inherit;
    transition: all 0.15s;
}

.format-btn:hover {
    border-color: #4a90d9;
    color: #4a90d9;
}

.format-btn.active {
    background: #4a90d9;
    color: white;
    border-color: #4a90d9;
}

.output-section {
    background: white;
    border-radius: 12px;
    padding: 1.5rem;
    box-shadow: 0 1px 3px rgba(0,0,0,0.08), 0 1px 2px rgba(0,0,0,0.06);
    margin-bottom: 1rem;
}

.output-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 0.75rem;
}

.output-title {
    font-size: 0.875rem;
    font-weight: 600;
    color: #374151;
}

.copy-btn {
    padding: 5px 12px;
    background: #f8fafc;
    border: 1px solid #e2e8f0;
    border-radius: 6px;
    color: #475569;
    cursor: pointer;
    font-size: 12px;
    font-family: inherit;
    transition: all 0.15s;
}

.copy-btn:hover {
    background: #e2e8f0;
}

.output-textarea {
    width: 100%;
    min-height: 120px;
    padding: 1rem;
    background: #1e293b;
    color: #e2e8f0;
    border: none;
    border-radius: 8px;
    font-family: 'SF Mono', 'Fira Code', 'Fira Mono', 'Roboto Mono', monospace;
    font-size: 13px;
    line-height: 1.6;
    resize: vertical;
    outline: none;
}

.output-textarea:focus {
    box-shadow: 0 0 0 2px rgba(74, 144, 217, 0.3);
}

.rendered-table-wrapper {
    overflow-x: auto;
}

.rendered-table {
    width: 100%;
    border-collapse: collapse;
    font-size: 14px;
}

.rendered-table th,
.rendered-table td {
    padding: 8px 12px;
    border: 1px solid #e5e7eb;
}

.rendered-table thead th {
    background: #f8fafc;
    font-weight: 600;
    color: #374151;
    border-bottom: 2px solid #d1d5db;
}

.rendered-table tbody tr:nth-child(even) {
    background: #f9fafb;
}

.rendered-table tbody tr:hover {
    background: #f0f4ff;
}

.import-section {
    margin-bottom: 1rem;
    padding: 1rem;
    background: #f8fafc;
    border: 1px dashed #cbd5e1;
    border-radius: 8px;
}

.import-textarea {
    width: 100%;
    min-height: 100px;
    padding: 0.75rem;
    border: 1px solid #e5e7eb;
    border-radius: 6px;
    font-family: 'SF Mono', 'Fira Code', 'Fira Mono', 'Roboto Mono', monospace;
    font-size: 13px;
    line-height: 1.5;
    resize: vertical;
    outline: none;
    background: white;
}

.import-textarea:focus {
    border-color: #4a90d9;
    box-shadow: 0 0 0 3px rgba(74, 144, 217, 0.12);
}

.import-actions {
    display: flex;
    gap: 8px;
    margin-top: 8px;
}

.hsep-row td {
    padding: 0 !important;
}

.hsep-btn {
    display: block;
    width: 100%;
    padding: 0;
    margin: 0;
    border: none;
    background: transparent;
    cursor: pointer;
    height: 12px;
    position: relative;
}

.hsep-btn:hover {
    background: #f0f4ff;
}

.hsep-indicator {
    position: absolute;
    top: 50%;
    left: 8px;
    right: 8px;
    height: 0;
}

.vsep-btn {
    width: 28px;
    height: 28px;
    border: 1px solid #e5e7eb;
    border-radius: 6px;
    background: white;
    cursor: pointer;
    font-size: 16px;
    font-family: 'SF Mono', 'Fira Code', 'Fira Mono', 'Roboto Mono', monospace;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    color: #4a90d9;
    transition: all 0.15s;
    line-height: 1;
}

.vsep-btn:hover {
    background: #f0f4ff;
    border-color: #4a90d9;
}

.vsep-cell {
    width: 18px;
    min-width: 18px;
    max-width: 18px;
    padding: 0 !important;
    text-align: center;
}

.vsep-inline-btn {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 18px;
    height: 100%;
    min-height: 32px;
    border: none;
    background: transparent;
    cursor: pointer;
    font-size: 14px;
    font-family: 'SF Mono', 'Fira Code', 'Fira Mono', 'Roboto Mono', monospace;
    color: #4a90d9;
    transition: background 0.15s;
    margin: 0 auto;
    padding: 0;
    line-height: 1;
}

.vsep-inline-btn:hover {
    background: #f0f4ff;
}

.hsep-setall-btn {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 22px;
    height: 12px;
    border: none;
    background: transparent;
    cursor: pointer;
    font-size: 14px;
    font-family: 'SF Mono', 'Fira Code', 'Fira Mono', 'Roboto Mono', monospace;
    color: #4a90d9;
    transition: all 0.15s;
    padding: 0;
    line-height: 1;
}

.hsep-setall-btn:hover {
    color: #4a90d9;
    background: #f0f4ff;
}

.vsep-none {
    color: #d0d0d0 !important;
    font-size: 11px !important;
}

.hsep-none {
    opacity: 0.5;
}

.box-textarea {
    line-height: 1.0;
}

.output-header.collapsed {
    margin-bottom: 0;
}

.section-toggle {
    background: none;
    border: none;
    padding: 0;
    cursor: pointer;
    user-select: none;
    font-size: 0.875rem;
    font-weight: 600;
    color: #374151;
    font-family: inherit;
    line-height: inherit;
}

.section-chevron {
    display: inline-block;
    margin-right: 0.4em;
    font-size: 0.75rem;
    transition: transform 0.15s;
}

.section-chevron.collapsed {
    transform: rotate(-90deg);
}

.sort-controls {
    display: flex;
    align-items: center;
    gap: 10px;
    flex-wrap: wrap;
    margin-top: 12px;
}

.sort-controls select {
    padding: 6px 10px;
    border: 1px solid #e5e7eb;
    border-radius: 8px;
    font-size: 13px;
    font-family: inherit;
    background: white;
    color: #374151;
    cursor: pointer;
    transition: border-color 0.15s;
}

.sort-controls select:hover {
    border-color: #4a90d9;
}

.sort-controls fieldset {
    display: inline-flex;
    align-items: center;
    border: none;
    padding: 0;
    margin: 0;
}

.sort-controls legend {
    position: absolute;
    width: 1px;
    height: 1px;
    padding: 0;
    margin: -1px;
    overflow: hidden;
    clip: rect(0, 0, 0, 0);
    white-space: nowrap;
    border: 0;
}

.sort-pill-group {
    display: inline-flex;
    border: 1px solid #e5e7eb;
    border-radius: 8px;
    overflow: hidden;
}

.sort-pill-group input[type="radio"] {
    position: absolute;
    width: 1px;
    height: 1px;
    padding: 0;
    margin: -1px;
    overflow: hidden;
    clip: rect(0, 0, 0, 0);
    white-space: nowrap;
    border: 0;
}

.sort-pill {
    display: inline-flex;
    align-items: center;
    gap: 4px;
    padding: 5px 12px;
    border: none;
    border-right: 1px solid #e5e7eb;
    background: white;
    color: #6b7280;
    cursor: pointer;
    font-size: 13px;
    font-family: inherit;
    font-weight: 500;
    transition: all 0.15s;
    line-height: 1.2;
}

.sort-pill:last-child {
    border-right: none;
}

.sort-pill:hover {
    background: #f0f4ff;
    color: #4a90d9;
}

.sort-pill.active {
    background: #4a90d9;
    color: white;
}

.sort-pill svg {
    display: block;
}

.summary-controls {
    display: flex;
    align-items: center;
    gap: 10px;
    margin-top: 12px;
}

.sort-controls-label {
    font-size: 13px;
    color: #6b7280;
    font-weight: 500;
}

.summary-row td,
.summary-row th {
    background: #f3f4f6 !important;
}

.summary-row .cell-input {
    background: #f3f4f6;
    color: #6b7280;
    font-weight: 600;
    cursor: default;
}

.rendered-table tfoot td,
.rendered-table tfoot th {
    background: #f3f4f6;
    color: #6b7280;
    font-weight: 600;
}
"""
        ]


viewHeader : Html msg
viewHeader =
    div [ Attr.class "header" ]
        [ h1 [] [ text "Tabular" ]
        , p [] [ text "Markdown Table Editor" ]
        ]


viewTableEditor : Model -> Html FrontendMsg
viewTableEditor model =
    let
        colRange =
            List.range 0 (model.cols - 1)

        canDeleteCol =
            model.cols > 1

        canDeleteRow =
            model.rows > 1

        colPillRow =
            tr []
                (td [] []
                    :: List.concatMap
                        (\c ->
                            let
                                pillTd =
                                    td [ Attr.style "text-align" "center" ]
                                        [ div [ Attr.class "btn-pill" ]
                                            [ button
                                                [ Attr.id ("insert-col-" ++ String.fromInt c)
                                                , Attr.class "pill-add-btn"
                                                , Attr.title ("Insert column before column " ++ String.fromInt (c + 1))
                                                , onClick (InsertColumn c)
                                                ]
                                                [ text "+" ]
                                            , button
                                                [ Attr.id ("del-col-" ++ String.fromInt c)
                                                , Attr.class "pill-del-btn"
                                                , Attr.title ("Remove column " ++ String.fromInt (c + 1))
                                                , onClick (RemoveColumn c)
                                                , Attr.disabled (not canDeleteCol)
                                                ]
                                                [ text "×" ]
                                            ]
                                        ]
                            in
                            if c < model.cols - 1 then
                                [ pillTd, td [ Attr.class "vsep-cell" ] [] ]

                            else
                                [ pillTd ]
                        )
                        colRange
                    ++ [ td [] [] ]
                )

        headerAlignmentRow =
            tr []
                (td [ Attr.style "text-align" "center" ] [ vsepButton 0 ]
                    :: List.concatMap
                        (\c ->
                            let
                                currentAlign =
                                    getHeaderAlignment c model.headerAlignments

                                alignBtn align idSuffix label icon =
                                    button
                                        [ Attr.id ("halign-" ++ String.fromInt c ++ "-" ++ idSuffix)
                                        , Attr.class
                                            (if currentAlign == align then
                                                "align-btn active"

                                             else
                                                "align-btn"
                                            )
                                        , onClick (SetHeaderAlignment c align)
                                        , Attr.title ("Header: " ++ label)
                                        ]
                                        [ icon ]

                                alignTd =
                                    td [ Attr.style "text-align" "center" ]
                                        [ div [ Attr.class "align-group" ]
                                            [ alignBtn AlignLeft "l" "Left" alignLeftIcon
                                            , alignBtn AlignCenter "c" "Center" alignCenterIcon
                                            , alignBtn AlignRight "r" "Right" alignRightIcon
                                            ]
                                        ]
                            in
                            if c < model.cols - 1 then
                                [ alignTd, td [ Attr.class "vsep-cell" ] [ vsepButton (c + 1) ] ]

                            else
                                [ alignTd ]
                        )
                        colRange
                    ++ [ td [ Attr.style "text-align" "center" ] [ vsepButton model.cols ] ]
                )

        bodyAlignmentRow =
            tr []
                (td [] []
                    :: List.concatMap
                        (\c ->
                            let
                                currentAlign =
                                    getBodyAlignment c model.bodyAlignments

                                alignBtn align idSuffix label icon =
                                    button
                                        [ Attr.id ("balign-" ++ String.fromInt c ++ "-" ++ idSuffix)
                                        , Attr.class
                                            (if currentAlign == align then
                                                "align-btn active"

                                             else
                                                "align-btn"
                                            )
                                        , onClick (SetBodyAlignment c align)
                                        , Attr.title ("Body: " ++ label)
                                        ]
                                        [ icon ]

                                alignTd =
                                    td [ Attr.style "text-align" "center" ]
                                        [ div [ Attr.class "align-group" ]
                                            [ alignBtn AlignLeft "l" "Left" alignLeftIcon
                                            , alignBtn AlignCenter "c" "Center" alignCenterIcon
                                            , alignBtn AlignRight "r" "Right" alignRightIcon
                                            ]
                                        ]
                            in
                            if c < model.cols - 1 then
                                [ alignTd, td [ Attr.class "vsep-cell" ] [] ]

                            else
                                [ alignTd ]
                        )
                        colRange
                    ++ [ td [] [] ]
                )

        hSepRow hIdx =
            let
                rowStyle =
                    getHorizontalLineStyle hIdx model.horizontalLineStyles
            in
            tr [ Attr.class "hsep-row" ]
                (td [ Attr.style "padding" "0" ]
                    [ button
                        [ Attr.class
                            (if rowStyle == None then
                                "hsep-setall-btn hsep-none"

                             else
                                "hsep-setall-btn"
                            )
                        , Attr.title (hSepLabel hIdx model.rows ++ ": " ++ lineStyleLabel rowStyle ++ " (set all)")
                        , onClick (CycleHorizontalLineStyle hIdx)
                        ]
                        [ text
                            (if rowStyle == None then
                                "Ø"

                             else
                                horizontalChar rowStyle
                            )
                        ]
                    ]
                    :: List.concatMap
                        (\c ->
                            let
                                effectiveStyle =
                                    getEffectiveHStyle hIdx c model.cellHorizontalStyles model.horizontalLineStyles

                                segmentTd =
                                    td [ Attr.style "padding" "0" ]
                                        [ button
                                            [ Attr.class
                                                (if effectiveStyle == None then
                                                    "hsep-btn hsep-none"

                                                 else
                                                    "hsep-btn"
                                                )
                                            , Attr.title (hSepLabel hIdx model.rows ++ " col " ++ String.fromInt (c + 1) ++ ": " ++ lineStyleLabel effectiveStyle)
                                            , onClick (CycleCellHorizontalStyle hIdx c)
                                            ]
                                            [ div
                                                [ Attr.class "hsep-indicator"
                                                , Attr.style "border-top"
                                                    (if effectiveStyle == None then
                                                        "1px dotted #d0d0d0"

                                                     else
                                                        lineStyleToCss effectiveStyle ++ " #4a90d9"
                                                    )
                                                ]
                                                []
                                            ]
                                        ]
                            in
                            if c < model.cols - 1 then
                                [ segmentTd, td [ Attr.class "vsep-cell", Attr.style "padding" "0" ] [] ]

                            else
                                [ segmentTd ]
                        )
                        colRange
                    ++ [ td [ Attr.style "padding" "0" ] [] ]
                )

        dataRow r =
            tr []
                (td [] []
                    :: List.concatMap
                        (\c ->
                            let
                                cellTd =
                                    td []
                                        [ input
                                            [ Attr.id ("cell-" ++ String.fromInt r ++ "-" ++ String.fromInt c)
                                            , Attr.class
                                                (if r == 0 then
                                                    "cell-input header-cell"

                                                 else
                                                    "cell-input"
                                                )
                                            , Attr.value (getCell r c model.cells)
                                            , Attr.placeholder
                                                (if r == 0 then
                                                    "Header " ++ String.fromInt (c + 1)

                                                 else
                                                    ""
                                                )
                                            , Attr.spellcheck False
                                            , onInput (CellChanged r c)
                                            ]
                                            []
                                        ]
                            in
                            if c < model.cols - 1 then
                                let
                                    vIdx =
                                        c + 1

                                    effectiveStyle =
                                        getEffectiveVStyle r vIdx model.cellVerticalStyles model.verticalLineStyles
                                in
                                [ cellTd
                                , td [ Attr.class "vsep-cell" ]
                                    [ button
                                        [ Attr.class
                                            (if effectiveStyle == None then
                                                "vsep-inline-btn vsep-none"

                                             else
                                                "vsep-inline-btn"
                                            )
                                        , Attr.title (vSepLabel vIdx model.cols ++ " row " ++ String.fromInt (r + 1) ++ ": " ++ lineStyleLabel effectiveStyle)
                                        , onClick (CycleCellVerticalStyle r vIdx)
                                        ]
                                        [ text
                                            (if effectiveStyle == None then
                                                "Ø"

                                             else
                                                verticalChar effectiveStyle
                                            )
                                        ]
                                    ]
                                ]

                            else
                                [ cellTd ]
                        )
                        colRange
                    ++ [ td [ Attr.style "vertical-align" "middle" ]
                            [ div [ Attr.class "btn-pill" ]
                                [ button
                                    [ Attr.id ("insert-row-" ++ String.fromInt r)
                                    , Attr.class "pill-add-btn"
                                    , Attr.title ("Insert row before row " ++ String.fromInt (r + 1))
                                    , onClick (InsertRow r)
                                    ]
                                    [ text "+" ]
                                , button
                                    [ Attr.id ("del-row-" ++ String.fromInt r)
                                    , Attr.class "pill-del-btn"
                                    , Attr.title ("Remove row " ++ String.fromInt (r + 1))
                                    , onClick (RemoveRow r)
                                    , Attr.disabled (not canDeleteRow)
                                    ]
                                    [ text "×" ]
                                ]
                            ]
                       ]
                )

        bodyRowOrder =
            List.range 1 (model.rows - 1)

        summaryRowEditor fn =
            let
                vals =
                    computeSummaryRow fn model.cols model.cells (List.range 1 (model.rows - 1))
            in
            tr [ Attr.class "summary-row" ]
                (td [] []
                    :: List.concatMap
                        (\c ->
                            let
                                cellVal =
                                    case List.head (List.drop c vals) of
                                        Just v ->
                                            v

                                        Nothing ->
                                            ""

                                cellTd =
                                    td []
                                        [ input
                                            [ Attr.class
                                                (if c == 0 then
                                                    "cell-input header-cell"

                                                 else
                                                    "cell-input"
                                                )
                                            , Attr.value cellVal
                                            , Attr.readonly True
                                            , Attr.tabindex -1
                                            ]
                                            []
                                        ]
                            in
                            if c < model.cols - 1 then
                                [ cellTd, td [ Attr.class "vsep-cell" ] [] ]

                            else
                                [ cellTd ]
                        )
                        colRange
                    ++ [ td [] [] ]
                )

        summaryHSepRow sepIdx =
            let
                sepStyle =
                    Dict.get sepIdx model.summarySeparatorStyles |> Maybe.withDefault Thin
            in
            tr [ Attr.class "hsep-row" ]
                (td [ Attr.style "padding" "0" ]
                    [ button
                        [ Attr.class
                            (if sepStyle == None then
                                "hsep-setall-btn hsep-none"

                             else
                                "hsep-setall-btn"
                            )
                        , Attr.title ("Summary separator " ++ String.fromInt (sepIdx + 1) ++ ": " ++ lineStyleLabel sepStyle)
                        , onClick (CycleSummarySeparatorStyle sepIdx)
                        ]
                        [ text
                            (if sepStyle == None then
                                "Ø"

                             else
                                horizontalChar sepStyle
                            )
                        ]
                    ]
                    :: List.concatMap
                        (\c ->
                            let
                                segmentTd =
                                    td [ Attr.style "padding" "0" ]
                                        [ button
                                            [ Attr.class
                                                (if sepStyle == None then
                                                    "hsep-btn hsep-none"

                                                 else
                                                    "hsep-btn"
                                                )
                                            , Attr.title ("Summary separator " ++ String.fromInt (sepIdx + 1) ++ ": " ++ lineStyleLabel sepStyle)
                                            , onClick (CycleSummarySeparatorStyle sepIdx)
                                            ]
                                            [ div
                                                [ Attr.class "hsep-indicator"
                                                , Attr.style "border-top"
                                                    (if sepStyle == None then
                                                        "1px dotted #d0d0d0"

                                                     else
                                                        lineStyleToCss sepStyle ++ " #4a90d9"
                                                    )
                                                ]
                                                []
                                            ]
                                        ]
                            in
                            if c < model.cols - 1 then
                                [ segmentTd, td [ Attr.class "vsep-cell", Attr.style "padding" "0" ] [] ]

                            else
                                [ segmentTd ]
                        )
                        colRange
                    ++ [ td [ Attr.style "padding" "0" ] [] ]
                )

        summaryEntries =
            let
                fns =
                    SeqSet.toList model.summaryRows
            in
            List.concat
                (List.indexedMap
                    (\i fn ->
                        if i > 0 then
                            [ ( "summary-hsep-" ++ String.fromInt (i - 1), summaryHSepRow (i - 1) )
                            , ( "summary-" ++ summaryLabel fn, summaryRowEditor fn )
                            ]

                        else
                            [ ( "summary-" ++ summaryLabel fn, summaryRowEditor fn ) ]
                    )
                    fns
                )

        keyedBodyRows =
            [ ( "hsep-0", hSepRow 0 ), ( "row-0", dataRow 0 ) ]
                ++ List.concatMap
                    (\r ->
                        [ ( "hsep-" ++ String.fromInt r, hSepRow r )
                        , ( "row-" ++ String.fromInt r, dataRow r )
                        ]
                    )
                    bodyRowOrder
                ++ [ ( "hsep-bottom", hSepRow model.rows ) ]
                ++ summaryEntries
                ++ [ ( "col-pills", colPillRow ) ]

        vsepButton vIdx =
            let
                style =
                    getVerticalLineStyle vIdx model.verticalLineStyles
            in
            button
                [ Attr.class
                    (if style == None then
                        "vsep-btn vsep-none"

                     else
                        "vsep-btn"
                    )
                , Attr.title (vSepLabel vIdx model.cols ++ ": " ++ lineStyleLabel style)
                , onClick (CycleVerticalLineStyle vIdx)
                ]
                [ text
                    (if style == None then
                        "Ø"

                     else
                        verticalChar style
                    )
                ]

    in
    div [ Attr.class "table-container" ]
        [ if model.showImport then
            div [ Attr.class "import-section" ]
                [ textarea
                    [ Attr.id "import-textarea"
                    , Attr.class "import-textarea"
                    , Attr.placeholder "Paste from Excel, Google Sheets, or CSV..."
                    , Attr.value model.importText
                    , onInput ImportTextChanged
                    , Attr.rows 6
                    ]
                    []
                , div [ Attr.class "import-actions" ]
                    [ button
                        [ Attr.id "import-btn"
                        , Attr.class "add-btn"
                        , onClick ImportData
                        , Attr.disabled (String.isEmpty (String.trim model.importText))
                        ]
                        [ text "Import" ]
                    , button [ Attr.id "import-cancel", Attr.class "add-btn", onClick ToggleImport ]
                        [ text "Cancel" ]
                    ]
                ]

          else
            text ""
        , table [ Attr.class "editor-table" ]
            [ thead [] [ headerAlignmentRow, bodyAlignmentRow ]
            , Html.Keyed.node "tbody" [] keyedBodyRows
            ]
        , div [ Attr.class "button-row" ]
            [ button [ Attr.id "add-row", Attr.class "add-btn", onClick AddRow ]
                [ text "+ Row" ]
            , button [ Attr.id "add-column", Attr.class "add-btn", onClick AddColumn ]
                [ text "+ Column" ]
            , button [ Attr.id "toggle-import", Attr.class "add-btn", onClick ToggleImport ]
                [ text
                    (if model.showImport then
                        "Hide Import"

                     else
                        "Import Data"
                    )
                ]
            , button
                [ Attr.id "undo-btn"
                , Attr.class "add-btn"
                , onClick Undo
                , Attr.disabled (List.isEmpty model.undoStack)
                ]
                [ text "Undo" ]
            ]
        , viewSortControls model
        , viewSummaryControls model
        ]


viewSortControls : Model -> Html FrontendMsg
viewSortControls model =
    let
        colRange =
            List.range 0 (model.cols - 1)

        selectedCol =
            case model.sortState of
                SortedBy col _ _ ->
                    String.fromInt col

                Unsorted ->
                    ""

        columnOption c =
            let
                headerText =
                    getCell 0 c model.cells

                label =
                    if String.isEmpty (String.trim headerText) then
                        "Column " ++ String.fromInt (c + 1)

                    else
                        headerText
            in
            option [ Attr.value (String.fromInt c) ] [ text label ]

        sortOptions =
            case model.sortState of
                Unsorted ->
                    []

                SortedBy _ dir method ->
                    [ button
                        [ Attr.id "apply-sort-to-inputs"
                        , Attr.class "add-btn"
                        , onClick ApplySortToInputs
                        ]
                        [ text "Sort inputs to match outputs" ]
                    , fieldset []
                        [ legend [] [ text "Direction" ]
                        , div [ Attr.class "sort-pill-group" ]
                            [ label
                                [ Attr.class
                                    (if dir == Ascending then
                                        "sort-pill active"

                                     else
                                        "sort-pill"
                                    )
                                ]
                                [ input
                                    [ Attr.type_ "radio"
                                    , Attr.name "sort-direction"
                                    , Attr.id "sort-asc"
                                    , Attr.checked (dir == Ascending)
                                    , onClick (SetSortDirection Ascending)
                                    ]
                                    []
                                , sortAscIcon
                                , text "Asc"
                                ]
                            , label
                                [ Attr.class
                                    (if dir == Descending then
                                        "sort-pill active"

                                     else
                                        "sort-pill"
                                    )
                                ]
                                [ input
                                    [ Attr.type_ "radio"
                                    , Attr.name "sort-direction"
                                    , Attr.id "sort-desc"
                                    , Attr.checked (dir == Descending)
                                    , onClick (SetSortDirection Descending)
                                    ]
                                    []
                                , sortDescIcon
                                , text "Desc"
                                ]
                            ]
                        ]
                    , fieldset []
                        [ legend [] [ text "Method" ]
                        , div [ Attr.class "sort-pill-group" ]
                            [ label
                                [ Attr.class
                                    (if method == Lexicographic then
                                        "sort-pill active"

                                     else
                                        "sort-pill"
                                    )
                                ]
                                [ input
                                    [ Attr.type_ "radio"
                                    , Attr.name "sort-method"
                                    , Attr.id "sort-lex"
                                    , Attr.checked (method == Lexicographic)
                                    , onClick (SetSortMethod Lexicographic)
                                    ]
                                    []
                                , text "Text"
                                ]
                            , label
                                [ Attr.class
                                    (if method == Numeric then
                                        "sort-pill active"

                                     else
                                        "sort-pill"
                                    )
                                ]
                                [ input
                                    [ Attr.type_ "radio"
                                    , Attr.name "sort-method"
                                    , Attr.id "sort-num"
                                    , Attr.checked (method == Numeric)
                                    , onClick (SetSortMethod Numeric)
                                    ]
                                    []
                                , text "Numeric"
                                ]
                            ]
                        ]
                    ]
    in
    div [ Attr.class "sort-controls" ]
        (select
            [ Attr.id "sort-column"
            , Attr.value selectedCol
            , onInput SetSortColumn
            ]
            (option [ Attr.value "" ] [ text "Sort\u{2026}" ]
                :: List.map columnOption colRange
            )
            :: sortOptions
        )


viewSummaryControls : Model -> Html FrontendMsg
viewSummaryControls model =
    let
        isActive fn =
            SeqSet.member fn model.summaryRows
    in
    div [ Attr.class "summary-controls" ]
        [ span [ Attr.class "sort-controls-label" ] [ text "Summary:" ]
        , button
            [ Attr.id "summary-max"
            , Attr.class
                (if isActive SummaryMax then
                    "sort-pill active"

                 else
                    "sort-pill"
                )
            , onClick (ToggleSummaryRow SummaryMax)
            ]
            [ text "Max" ]
        ]


viewMarkdownOutput : Model -> Html FrontendMsg
viewMarkdownOutput model =
    let
        collapsed =
            SeqSet.member MarkdownSection model.collapsedSections

        markdown =
            generateMarkdown model.outputFormat model.rows model.cols model.cells model.headerAlignments model.bodyAlignments (sortedBodyRows model) (SeqSet.toList model.summaryRows)
    in
    div [ Attr.class "output-section" ]
        (div
            [ Attr.class
                (if collapsed then
                    "output-header collapsed"

                 else
                    "output-header"
                )
            ]
            [ button [ Attr.class "output-title section-toggle", onClick (ToggleSection MarkdownSection) ]
                [ span
                    [ Attr.class
                        (if collapsed then
                            "section-chevron collapsed"

                         else
                            "section-chevron"
                        )
                    ]
                    [ text "▼" ]
                , text "Markdown"
                ]
            , div [ Attr.class "output-controls" ]
                [ button
                    [ Attr.id "format-compact"
                    , Attr.class
                        (if model.outputFormat == Compact then
                            "format-btn active"

                         else
                            "format-btn"
                        )
                    , onClick (SetOutputFormat Compact)
                    ]
                    [ text "Compact" ]
                , button
                    [ Attr.id "format-expanded"
                    , Attr.class
                        (if model.outputFormat == Expanded then
                            "format-btn active"

                         else
                            "format-btn"
                        )
                    , onClick (SetOutputFormat Expanded)
                    ]
                    [ text "Expanded" ]
                , Html.node "copy-button" [ Attr.attribute "target" "md-output" ] []
                ]
            ]
            :: (if collapsed then
                    []

                else
                    [ textarea
                        [ Attr.class "output-textarea"
                        , Attr.id "md-output"
                        , Attr.readonly True
                        , Attr.value markdown
                        , Attr.rows (max 4 (model.rows + 2))
                        ]
                        []
                    ]
               )
        )


viewRenderedTable : Model -> Html FrontendMsg
viewRenderedTable model =
    let
        colRange =
            List.range 0 (model.cols - 1)

        cellAttrs r c =
            let
                align =
                    if r == 0 then
                        getHeaderAlignment c model.headerAlignments

                    else
                        getBodyAlignment c model.bodyAlignments

                top =
                    getEffectiveHStyle r c model.cellHorizontalStyles model.horizontalLineStyles

                bottom =
                    getEffectiveHStyle (r + 1) c model.cellHorizontalStyles model.horizontalLineStyles

                left =
                    getEffectiveVStyle r c model.cellVerticalStyles model.verticalLineStyles

                right =
                    getEffectiveVStyle r (c + 1) model.cellVerticalStyles model.verticalLineStyles

                borderAttrs =
                    List.filterMap identity
                        [ if top /= Thin then
                            Just (Attr.style "border-top" (lineStyleToCss top ++ " #e5e7eb"))

                          else
                            Nothing
                        , if bottom /= Thin then
                            Just (Attr.style "border-bottom" (lineStyleToCss bottom ++ " #e5e7eb"))

                          else
                            Nothing
                        , if left /= Thin then
                            Just (Attr.style "border-left" (lineStyleToCss left ++ " #e5e7eb"))

                          else
                            Nothing
                        , if right /= Thin then
                            Just (Attr.style "border-right" (lineStyleToCss right ++ " #e5e7eb"))

                          else
                            Nothing
                        ]
            in
            Attr.style "text-align" (alignmentToStyle align) :: borderAttrs

        headerRow =
            tr []
                (List.map
                    (\c ->
                        th (cellAttrs 0 c) [ text (getCell 0 c model.cells) ]
                    )
                    colRange
                )

        keyedBodyRows =
            List.map
                (\r ->
                    ( "preview-row-" ++ String.fromInt r
                    , tr []
                        (List.map
                            (\c ->
                                td (cellAttrs r c) [ text (getCell r c model.cells) ]
                            )
                            colRange
                        )
                    )
                )
                (sortedBodyRows model)

        summaryFunctions =
            SeqSet.toList model.summaryRows

        bodyRowIndicesForSummary =
            List.range 1 (model.rows - 1)

        previewTfoot =
            if List.isEmpty summaryFunctions then
                text ""

            else
                tfoot []
                    (List.map
                        (\fn ->
                            let
                                vals =
                                    computeSummaryRow fn model.cols model.cells bodyRowIndicesForSummary
                            in
                            tr [ Attr.class "summary-row" ]
                                (List.indexedMap
                                    (\c v ->
                                        if c == 0 then
                                            th [ Attr.style "text-align" "left" ] [ text v ]

                                        else
                                            td [] [ text v ]
                                    )
                                    vals
                                )
                        )
                        summaryFunctions
                    )

        collapsed =
            SeqSet.member PreviewSection model.collapsedSections
    in
    div [ Attr.class "output-section" ]
        (div
            [ Attr.class
                (if collapsed then
                    "output-header collapsed"

                 else
                    "output-header"
                )
            ]
            [ button [ Attr.class "output-title section-toggle", onClick (ToggleSection PreviewSection) ]
                [ span
                    [ Attr.class
                        (if collapsed then
                            "section-chevron collapsed"

                         else
                            "section-chevron"
                        )
                    ]
                    [ text "▼" ]
                , text "Preview"
                ]
            ]
            :: (if collapsed then
                    []

                else
                    [ div [ Attr.class "rendered-table-wrapper" ]
                        [ table [ Attr.class "rendered-table" ]
                            [ thead [] [ headerRow ]
                            , Html.Keyed.node "tbody" [] keyedBodyRows
                            , previewTfoot
                            ]
                        ]
                    ]
               )
        )


viewHtmlTableOutput : Model -> Html FrontendMsg
viewHtmlTableOutput model =
    let
        collapsed =
            SeqSet.member HtmlSection model.collapsedSections

        htmlTable =
            generateHtmlTable model.rows model.cols model.cells model.headerAlignments model.bodyAlignments model.horizontalLineStyles model.verticalLineStyles model.cellHorizontalStyles model.cellVerticalStyles (sortedBodyRows model) (SeqSet.toList model.summaryRows)
    in
    div [ Attr.class "output-section" ]
        (div
            [ Attr.class
                (if collapsed then
                    "output-header collapsed"

                 else
                    "output-header"
                )
            ]
            [ button [ Attr.class "output-title section-toggle", onClick (ToggleSection HtmlSection) ]
                [ span
                    [ Attr.class
                        (if collapsed then
                            "section-chevron collapsed"

                         else
                            "section-chevron"
                        )
                    ]
                    [ text "▼" ]
                , text "HTML Table"
                ]
            , Html.node "copy-button" [ Attr.attribute "target" "html-output" ] []
            ]
            :: (if collapsed then
                    []

                else
                    [ textarea
                        [ Attr.class "output-textarea"
                        , Attr.id "html-output"
                        , Attr.readonly True
                        , Attr.value htmlTable
                        , Attr.rows (max 4 (model.rows + 6))
                        ]
                        []
                    ]
               )
        )


viewBoxDrawingOutput : Model -> Html FrontendMsg
viewBoxDrawingOutput model =
    let
        collapsed =
            SeqSet.member BoxDrawingSection model.collapsedSections

        boxDrawing =
            generateBoxDrawing model.rows model.cols model.cells model.headerAlignments model.bodyAlignments model.horizontalLineStyles model.verticalLineStyles model.cellHorizontalStyles model.cellVerticalStyles (sortedBodyRows model) (SeqSet.toList model.summaryRows) model.summarySeparatorStyles
    in
    div [ Attr.class "output-section" ]
        (div
            [ Attr.class
                (if collapsed then
                    "output-header collapsed"

                 else
                    "output-header"
                )
            ]
            [ button [ Attr.class "output-title section-toggle", onClick (ToggleSection BoxDrawingSection) ]
                [ span
                    [ Attr.class
                        (if collapsed then
                            "section-chevron collapsed"

                         else
                            "section-chevron"
                        )
                    ]
                    [ text "▼" ]
                , text "Box Drawing"
                ]
            , Html.node "copy-button" [ Attr.attribute "target" "box-output" ] []
            ]
            :: (if collapsed then
                    []

                else
                    [ textarea
                        [ Attr.class "output-textarea box-textarea"
                        , Attr.id "box-output"
                        , Attr.readonly True
                        , Attr.value boxDrawing
                        , Attr.rows (max 4 (model.rows * 2 + 1))
                        ]
                        []
                    ]
               )
        )
