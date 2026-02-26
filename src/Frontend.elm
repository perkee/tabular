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
import Index exposing (..)
import Lamdera
import SeqSet
import Svg
import Svg.Attributes as SvgAttr
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
      , rows = count 3
      , cols = count 3
      , cells =
            fromList2
                [ ( index 0, index 0, "Header 1" )
                , ( index 0, index 1, "Header 2" )
                , ( index 0, index 2, "Header 3" )
                ]
      , headerAlignments = empty
      , bodyAlignments = empty
      , horizontalLineStyles = empty
      , verticalLineStyles = empty
      , cellHorizontalStyles = empty2
      , cellVerticalStyles = empty2
      , outputFormat = Expanded
      , showImport = False
      , importText = ""
      , collapsedSections = SeqSet.empty
      , undoStack = []
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
            ( { model | cells = insert2 row col value model.cells }
            , Command.none
            )

        AddRow ->
            ( { model | rows = countPlusOne model.rows, undoStack = pushUndo model }, Command.none )

        AddColumn ->
            ( { model | cols = countPlusOne model.cols, undoStack = pushUndo model }, Command.none )

        RemoveRow rowIndex ->
            if countIsGreaterThan 1 model.rows then
                ( { model
                    | rows = countMinusOne model.rows
                    , cells = removeRow rowIndex model.cells
                    , horizontalLineStyles = removeIndexFromDict (hLineBelow rowIndex) model.horizontalLineStyles
                    , cellHorizontalStyles = removeCellStyleRow rowIndex model.cellHorizontalStyles
                    , cellVerticalStyles = removeCellVStyleRow rowIndex model.cellVerticalStyles
                    , undoStack = pushUndo model
                  }
                , Command.none
                )

            else
                ( model, Command.none )

        RemoveColumn colIndex ->
            if countIsGreaterThan 1 model.cols then
                ( { model
                    | cols = countMinusOne model.cols
                    , cells = removeColumn colIndex model.cells
                    , headerAlignments = removeColumnAlignments colIndex model.headerAlignments
                    , bodyAlignments = removeColumnAlignments colIndex model.bodyAlignments
                    , verticalLineStyles = removeIndexFromDict (vLineRight colIndex) model.verticalLineStyles
                    , cellHorizontalStyles = removeCellStyleCol colIndex model.cellHorizontalStyles
                    , cellVerticalStyles = removeCellVStyleCol colIndex model.cellVerticalStyles
                    , undoStack = pushUndo model
                  }
                , Command.none
                )

            else
                ( model, Command.none )

        InsertRow rowIdx ->
            ( { model
                | rows = countPlusOne model.rows
                , cells = insertRowCells rowIdx model.cells
                , horizontalLineStyles = insertIndexIntoDict (hLineBelow rowIdx) model.horizontalLineStyles
                , cellHorizontalStyles = insertCellStyleRow rowIdx model.cellHorizontalStyles
                , cellVerticalStyles = insertCellVStyleRow rowIdx model.cellVerticalStyles
                , undoStack = pushUndo model
              }
            , Command.none
            )

        InsertColumn colIdx ->
            ( { model
                | cols = countPlusOne model.cols
                , cells = insertColumnCells colIdx model.cells
                , headerAlignments = insertColumnAlignments colIdx model.headerAlignments
                , bodyAlignments = insertColumnAlignments colIdx model.bodyAlignments
                , verticalLineStyles = insertIndexIntoDict (vLineRight colIdx) model.verticalLineStyles
                , cellHorizontalStyles = insertCellStyleCol colIdx model.cellHorizontalStyles
                , cellVerticalStyles = insertCellVStyleCol colIdx model.cellVerticalStyles
                , undoStack = pushUndo model
              }
            , Command.none
            )

        SetOutputFormat format ->
            ( { model | outputFormat = format }, Command.none )

        SetHeaderAlignment col alignment ->
            ( { model | headerAlignments = Index.insert col alignment model.headerAlignments }
            , Command.none
            )

        SetBodyAlignment col alignment ->
            ( { model | bodyAlignments = Index.insert col alignment model.bodyAlignments }
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
            if countIsGreaterThan 0 parsed.rows && countIsGreaterThan 0 parsed.cols then
                ( { model
                    | rows = parsed.rows
                    , cols = parsed.cols
                    , cells = parsed.cells
                    , headerAlignments = empty
                    , bodyAlignments = empty
                    , horizontalLineStyles = empty
                    , verticalLineStyles = empty
                    , cellHorizontalStyles = empty2
                    , cellVerticalStyles = empty2
                    , showImport = False
                    , importText = ""
                    , undoStack = pushUndo model
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
                        |> filter2 (\h _ _ -> toInt h /= toInt idx)
            in
            ( { model
                | horizontalLineStyles = Index.insert idx (cycleLineStyle current) model.horizontalLineStyles
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
                        |> filter2 (\_ v _ -> toInt v /= toInt idx)
            in
            ( { model
                | verticalLineStyles = Index.insert idx (cycleLineStyle current) model.verticalLineStyles
                , cellVerticalStyles = clearedCellStyles
              }
            , Command.none
            )

        CycleCellHorizontalStyle hIdx col ->
            let
                current =
                    getEffectiveHStyle hIdx col model.cellHorizontalStyles model.horizontalLineStyles
            in
            ( { model | cellHorizontalStyles = insert2 hIdx col (cycleLineStyle current) model.cellHorizontalStyles }
            , Command.none
            )

        CycleCellVerticalStyle row vIdx ->
            let
                current =
                    getEffectiveVStyle row vIdx model.cellVerticalStyles model.verticalLineStyles
            in
            ( { model | cellVerticalStyles = insert2 row vIdx (cycleLineStyle current) model.cellVerticalStyles }
            , Command.none
            )

        Undo ->
            case model.undoStack of
                s :: rest ->
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
                      }
                    , Command.none
                    )

                [] ->
                    ( model, Command.none )

        ToggleSection section ->
            let
                newSections =
                    if SeqSet.member section model.collapsedSections then
                        SeqSet.remove section model.collapsedSections

                    else
                        SeqSet.insert section model.collapsedSections
            in
            ( { model | collapsedSections = newSections }, Command.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Command FrontendOnly ToBackend FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Command.none )



-- HELPERS


removeRow : RowIndex -> IndexDict2 Row_ Column_ String -> IndexDict2 Row_ Column_ String
removeRow rowToRemove cells =
    cells
        |> toList2
        |> List.filterMap
            (\( r, c, v ) ->
                if toInt r == toInt rowToRemove then
                    Nothing

                else if toInt r > toInt rowToRemove then
                    Just ( prev r, c, v )

                else
                    Just ( r, c, v )
            )
        |> fromList2


removeColumn : ColumnIndex -> IndexDict2 Row_ Column_ String -> IndexDict2 Row_ Column_ String
removeColumn colToRemove cells =
    cells
        |> toList2
        |> List.filterMap
            (\( r, c, v ) ->
                if toInt c == toInt colToRemove then
                    Nothing

                else if toInt c > toInt colToRemove then
                    Just ( r, prev c, v )

                else
                    Just ( r, c, v )
            )
        |> fromList2


getCell : RowIndex -> ColumnIndex -> IndexDict2 Row_ Column_ String -> String
getCell row col cells =
    get2 row col cells |> Maybe.withDefault ""


removeColumnAlignments : ColumnIndex -> IndexDict Column_ Alignment -> IndexDict Column_ Alignment
removeColumnAlignments colToRemove alignments =
    alignments
        |> Index.toList
        |> List.filterMap
            (\( c, a ) ->
                if toInt c == toInt colToRemove then
                    Nothing

                else if toInt c > toInt colToRemove then
                    Just ( prev c, a )

                else
                    Just ( c, a )
            )
        |> Index.fromList


getHeaderAlignment : ColumnIndex -> IndexDict Column_ Alignment -> Alignment
getHeaderAlignment col alignments =
    Index.get col alignments |> Maybe.withDefault AlignCenter


getBodyAlignment : ColumnIndex -> IndexDict Column_ Alignment -> Alignment
getBodyAlignment col alignments =
    Index.get col alignments |> Maybe.withDefault AlignLeft


removeIndexFromDict : Index a -> IndexDict a v -> IndexDict a v
removeIndexFromDict idx dict =
    dict
        |> Index.toList
        |> List.filterMap
            (\( k, v ) ->
                if toInt k == toInt idx then
                    Nothing

                else if toInt k > toInt idx then
                    Just ( prev k, v )

                else
                    Just ( k, v )
            )
        |> Index.fromList


removeCellStyleRow : RowIndex -> IndexDict2 HLine_ Column_ LineStyle -> IndexDict2 HLine_ Column_ LineStyle
removeCellStyleRow rowIdx dict =
    let
        hAbove =
            toInt (hLineAbove rowIdx)

        hBelow =
            toInt (hLineBelow rowIdx)
    in
    dict
        |> toList2
        |> List.filterMap
            (\( h, c, v ) ->
                if toInt h == hAbove || toInt h == hBelow then
                    Nothing

                else if toInt h > hBelow then
                    Just ( prev h, c, v )

                else
                    Just ( h, c, v )
            )
        |> fromList2


removeCellVStyleRow : RowIndex -> IndexDict2 Row_ VLine_ LineStyle -> IndexDict2 Row_ VLine_ LineStyle
removeCellVStyleRow rowIdx dict =
    dict
        |> toList2
        |> List.filterMap
            (\( r, vi, v ) ->
                if toInt r == toInt rowIdx then
                    Nothing

                else if toInt r > toInt rowIdx then
                    Just ( prev r, vi, v )

                else
                    Just ( r, vi, v )
            )
        |> fromList2


removeCellStyleCol : ColumnIndex -> IndexDict2 HLine_ Column_ LineStyle -> IndexDict2 HLine_ Column_ LineStyle
removeCellStyleCol colIdx dict =
    dict
        |> toList2
        |> List.filterMap
            (\( h, c, v ) ->
                if toInt c == toInt colIdx then
                    Nothing

                else if toInt c > toInt colIdx then
                    Just ( h, prev c, v )

                else
                    Just ( h, c, v )
            )
        |> fromList2


removeCellVStyleCol : ColumnIndex -> IndexDict2 Row_ VLine_ LineStyle -> IndexDict2 Row_ VLine_ LineStyle
removeCellVStyleCol colIdx dict =
    let
        vLeft =
            toInt (vLineLeft colIdx)

        vRight =
            toInt (vLineRight colIdx)
    in
    dict
        |> toList2
        |> List.filterMap
            (\( r, vi, v ) ->
                if toInt vi == vLeft || toInt vi == vRight then
                    Nothing

                else if toInt vi > vRight then
                    Just ( r, prev vi, v )

                else
                    Just ( r, vi, v )
            )
        |> fromList2


insertRowCells : RowIndex -> IndexDict2 Row_ Column_ String -> IndexDict2 Row_ Column_ String
insertRowCells idx cells =
    cells
        |> toList2
        |> List.map
            (\( r, c, v ) ->
                if toInt r >= toInt idx then
                    ( next r, c, v )

                else
                    ( r, c, v )
            )
        |> fromList2


insertColumnCells : ColumnIndex -> IndexDict2 Row_ Column_ String -> IndexDict2 Row_ Column_ String
insertColumnCells idx cells =
    cells
        |> toList2
        |> List.map
            (\( r, c, v ) ->
                if toInt c >= toInt idx then
                    ( r, next c, v )

                else
                    ( r, c, v )
            )
        |> fromList2


insertColumnAlignments : ColumnIndex -> IndexDict Column_ Alignment -> IndexDict Column_ Alignment
insertColumnAlignments idx alignments =
    alignments
        |> Index.toList
        |> List.map
            (\( c, a ) ->
                if toInt c >= toInt idx then
                    ( next c, a )

                else
                    ( c, a )
            )
        |> Index.fromList


insertIndexIntoDict : Index a -> IndexDict a v -> IndexDict a v
insertIndexIntoDict idx dict =
    dict
        |> Index.toList
        |> List.map
            (\( k, v ) ->
                if toInt k >= toInt idx then
                    ( next k, v )

                else
                    ( k, v )
            )
        |> Index.fromList


insertCellStyleRow : RowIndex -> IndexDict2 HLine_ Column_ LineStyle -> IndexDict2 HLine_ Column_ LineStyle
insertCellStyleRow rowIdx dict =
    dict
        |> toList2
        |> List.map
            (\( h, c, v ) ->
                if toInt h > toInt (hLineAbove rowIdx) then
                    ( next h, c, v )

                else
                    ( h, c, v )
            )
        |> fromList2


insertCellVStyleRow : RowIndex -> IndexDict2 Row_ VLine_ LineStyle -> IndexDict2 Row_ VLine_ LineStyle
insertCellVStyleRow rowIdx dict =
    dict
        |> toList2
        |> List.map
            (\( r, vi, v ) ->
                if toInt r >= toInt rowIdx then
                    ( next r, vi, v )

                else
                    ( r, vi, v )
            )
        |> fromList2


insertCellStyleCol : ColumnIndex -> IndexDict2 HLine_ Column_ LineStyle -> IndexDict2 HLine_ Column_ LineStyle
insertCellStyleCol colIdx dict =
    dict
        |> toList2
        |> List.map
            (\( h, c, v ) ->
                if toInt c >= toInt colIdx then
                    ( h, next c, v )

                else
                    ( h, c, v )
            )
        |> fromList2


insertCellVStyleCol : ColumnIndex -> IndexDict2 Row_ VLine_ LineStyle -> IndexDict2 Row_ VLine_ LineStyle
insertCellVStyleCol colIdx dict =
    dict
        |> toList2
        |> List.map
            (\( r, vi, v ) ->
                if toInt vi > toInt (vLineLeft colIdx) then
                    ( r, next vi, v )

                else
                    ( r, vi, v )
            )
        |> fromList2


getHorizontalLineStyle : HLineIndex -> IndexDict HLine_ LineStyle -> LineStyle
getHorizontalLineStyle idx styles =
    Index.get idx styles |> Maybe.withDefault Thin


getVerticalLineStyle : VLineIndex -> IndexDict VLine_ LineStyle -> LineStyle
getVerticalLineStyle idx styles =
    Index.get idx styles |> Maybe.withDefault Thin


getEffectiveHStyle : HLineIndex -> ColumnIndex -> IndexDict2 HLine_ Column_ LineStyle -> IndexDict HLine_ LineStyle -> LineStyle
getEffectiveHStyle hIdx col cellStyles rowStyles =
    case get2 hIdx col cellStyles of
        Just s ->
            s

        Nothing ->
            getHorizontalLineStyle hIdx rowStyles


getEffectiveVStyle : RowIndex -> VLineIndex -> IndexDict2 Row_ VLine_ LineStyle -> IndexDict VLine_ LineStyle -> LineStyle
getEffectiveVStyle row vIdx cellStyles colStyles =
    case get2 row vIdx cellStyles of
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


hSepLabel : HLineIndex -> RowCount -> String
hSepLabel idx rows =
    if toInt idx == 0 then
        "Top border"

    else if toInt idx == countToInt rows then
        "Bottom border"

    else
        "Row " ++ String.fromInt (toInt idx) ++ "-" ++ String.fromInt (toInt idx + 1)


vSepLabel : VLineIndex -> ColumnCount -> String
vSepLabel idx cols =
    if toInt idx == 0 then
        "Left border"

    else if toInt idx == countToInt cols then
        "Right border"

    else
        "Col " ++ String.fromInt (toInt idx) ++ "-" ++ String.fromInt (toInt idx + 1)


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


parseImportData : String -> { rows : RowCount, cols : ColumnCount, cells : IndexDict2 Row_ Column_ String }
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
                        List.indexedMap (\c val -> ( index r, index c, val )) row
                    )
                |> List.concat
                |> fromList2
    in
    { rows = count numRows, cols = count numCols, cells = cells }



-- MARKDOWN GENERATION


generateMarkdown : OutputFormat -> RowCount -> ColumnCount -> IndexDict2 Row_ Column_ String -> IndexDict Column_ Alignment -> IndexDict Column_ Alignment -> String
generateMarkdown format rows cols cells headerAlignments bodyAlignments =
    if countToInt rows == 0 || countToInt cols == 0 then
        ""

    else
        let
            colRange =
                rangeCount cols

            rowRange =
                rangeCount rows

            colWidths =
                List.map
                    (\c ->
                        List.foldl
                            (\r maxW ->
                                max maxW
                                    (String.length (escapePipe (getCell r c cells)))
                            )
                            3
                            rowRange
                    )
                    colRange

            formatRow r =
                let
                    rowAlignments =
                        if toInt r == 0 then
                            headerAlignments

                        else
                            bodyAlignments

                    getRowAlignment =
                        if toInt r == 0 then
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
                formatRow (index 0)

            dataRows =
                List.map formatRow (rangeFrom (index 1) (countMinusOne rows))
        in
        String.join "\n" (headerRow :: separatorRow :: dataRows)



-- BOX DRAWING GENERATION


generateBoxDrawing : RowCount -> ColumnCount -> IndexDict2 Row_ Column_ String -> IndexDict Column_ Alignment -> IndexDict Column_ Alignment -> IndexDict HLine_ LineStyle -> IndexDict VLine_ LineStyle -> IndexDict2 HLine_ Column_ LineStyle -> IndexDict2 Row_ VLine_ LineStyle -> String
generateBoxDrawing rows cols cells headerAlignments bodyAlignments hStyles vStyles cellHStyles cellVStyles =
    if countToInt rows == 0 || countToInt cols == 0 then
        ""

    else
        let
            colRange =
                rangeCount cols

            rowRange =
                rangeCount rows

            colWidths =
                List.map
                    (\c ->
                        List.foldl
                            (\r maxW -> max maxW (String.length (getCell r c cells)))
                            1
                            rowRange
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
                                        if toInt hIdx > 0 then
                                            lineStyleWeight (effV (index (toInt hIdx - 1)) vIdx)

                                        else
                                            WNone

                                    down =
                                        if toInt hIdx < countToInt rows then
                                            lineStyleWeight (effV (index (toInt hIdx)) vIdx)

                                        else
                                            WNone

                                    left =
                                        lineStyleWeight (effH hIdx (index (toInt vIdx - 1)))

                                    right =
                                        lineStyleWeight (effH hIdx (index (toInt vIdx)))
                                in
                                lookupCorner up down left right
                            )
                            (innerVLineRange cols)

                    leftCorner =
                        let
                            up =
                                if toInt hIdx > 0 then
                                    lineStyleWeight (effV (index (toInt hIdx - 1)) (index 0))

                                else
                                    WNone

                            down =
                                if toInt hIdx < countToInt rows then
                                    lineStyleWeight (effV (index (toInt hIdx)) (index 0))

                                else
                                    WNone
                        in
                        lookupCorner up down WNone (lineStyleWeight (effH hIdx (index 0)))

                    rightCorner =
                        let
                            up =
                                if toInt hIdx > 0 then
                                    lineStyleWeight (effV (index (toInt hIdx - 1)) (lastVLine cols))

                                else
                                    WNone

                            down =
                                if toInt hIdx < countToInt rows then
                                    lineStyleWeight (effV (index (toInt hIdx)) (lastVLine cols))

                                else
                                    WNone
                        in
                        lookupCorner up down (lineStyleWeight (effH hIdx (index (countToInt cols - 1)))) WNone
                in
                leftCorner ++ String.concat (interleave segments intersections) ++ rightCorner

            formatRow r =
                let
                    leftV =
                        verticalChar (effV r (index 0))

                    rightV =
                        verticalChar (effV r (lastVLine cols))

                    getRowAlignment =
                        if toInt r == 0 then
                            getHeaderAlignment

                        else
                            getBodyAlignment

                    rowAlignments =
                        if toInt r == 0 then
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
                            (innerVLineRange cols)
                in
                leftV ++ " " ++ String.concat (interleave cellTexts (List.map (\s -> " " ++ s ++ " ") innerSeps)) ++ " " ++ rightV

            allRows =
                List.concatMap
                    (\r ->
                        if toInt r == 0 then
                            [ horizontalLine (index 0), formatRow r ]

                        else
                            [ horizontalLine (index (toInt r)), formatRow r ]
                    )
                    rowRange
                    ++ [ horizontalLine (lastHLine rows) ]
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


generateHtmlTable : RowCount -> ColumnCount -> IndexDict2 Row_ Column_ String -> IndexDict Column_ Alignment -> IndexDict Column_ Alignment -> IndexDict HLine_ LineStyle -> IndexDict VLine_ LineStyle -> IndexDict2 HLine_ Column_ LineStyle -> IndexDict2 Row_ VLine_ LineStyle -> String
generateHtmlTable rows cols cells headerAlignments bodyAlignments hStyles vStyles cellHStyles cellVStyles =
    if countToInt rows == 0 || countToInt cols == 0 then
        ""

    else
        let
            colRange =
                rangeCount cols

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
                        getEffectiveHStyle (hLineAbove r) c cellHStyles hStyles

                    bottom =
                        getEffectiveHStyle (hLineBelow r) c cellHStyles hStyles

                    left =
                        getEffectiveVStyle r (vLineLeft c) cellVStyles vStyles

                    right =
                        getEffectiveVStyle r (vLineRight c) cellVStyles vStyles

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
                        indent 3 ++ "<th" ++ cellStyleAttr (index 0) c align ++ ">" ++ escapeHtml (getCell (index 0) c cells) ++ "</th>"
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
                List.concatMap bodyRow (rangeFrom (index 1) (countMinusOne rows))

            bodySection =
                if countIsGreaterThan 1 rows then
                    (indent 1 ++ "<tbody>") :: bodyRows ++ [ indent 1 ++ "</tbody>" ]

                else
                    []
        in
        String.join "\n" ("<table>" :: headerSection ++ bodySection ++ [ "</table>" ])



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
            rangeCount model.cols

        rowRange =
            rangeCount model.rows

        canDeleteCol =
            countIsGreaterThan 1 model.cols

        canDeleteRow =
            countIsGreaterThan 1 model.rows

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
                                                [ Attr.id ("insert-col-" ++ indexToString c)
                                                , Attr.class "pill-add-btn"
                                                , Attr.title ("Insert column before column " ++ String.fromInt (toInt c + 1))
                                                , onClick (InsertColumn c)
                                                ]
                                                [ text "+" ]
                                            , button
                                                [ Attr.id ("del-col-" ++ indexToString c)
                                                , Attr.class "pill-del-btn"
                                                , Attr.title ("Remove column " ++ String.fromInt (toInt c + 1))
                                                , onClick (RemoveColumn c)
                                                , Attr.disabled (not canDeleteCol)
                                                ]
                                                [ text "×" ]
                                            ]
                                        ]
                            in
                            if toInt c < countToInt model.cols - 1 then
                                [ pillTd, td [ Attr.class "vsep-cell" ] [] ]

                            else
                                [ pillTd ]
                        )
                        colRange
                    ++ [ td [] [] ]
                )

        headerAlignmentRow =
            tr []
                (td [ Attr.style "text-align" "center" ] [ vsepButton (index 0) ]
                    :: List.concatMap
                        (\c ->
                            let
                                currentAlign =
                                    getHeaderAlignment c model.headerAlignments

                                alignBtn align idSuffix label icon =
                                    button
                                        [ Attr.id ("halign-" ++ indexToString c ++ "-" ++ idSuffix)
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
                            if toInt c < countToInt model.cols - 1 then
                                [ alignTd, td [ Attr.class "vsep-cell" ] [ vsepButton (vLineRight c) ] ]

                            else
                                [ alignTd ]
                        )
                        colRange
                    ++ [ td [ Attr.style "text-align" "center" ] [ vsepButton (lastVLine model.cols) ] ]
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
                                        [ Attr.id ("balign-" ++ indexToString c ++ "-" ++ idSuffix)
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
                            if toInt c < countToInt model.cols - 1 then
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
                                            , Attr.title (hSepLabel hIdx model.rows ++ " col " ++ String.fromInt (toInt c + 1) ++ ": " ++ lineStyleLabel effectiveStyle)
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
                            if toInt c < countToInt model.cols - 1 then
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
                                            [ Attr.id ("cell-" ++ indexToString r ++ "-" ++ indexToString c)
                                            , Attr.class
                                                (if toInt r == 0 then
                                                    "cell-input header-cell"

                                                 else
                                                    "cell-input"
                                                )
                                            , Attr.value (getCell r c model.cells)
                                            , Attr.placeholder
                                                (if toInt r == 0 then
                                                    "Header " ++ String.fromInt (toInt c + 1)

                                                 else
                                                    ""
                                                )
                                            , Attr.spellcheck False
                                            , onInput (CellChanged r c)
                                            ]
                                            []
                                        ]
                            in
                            if toInt c < countToInt model.cols - 1 then
                                let
                                    vIdx =
                                        vLineRight c

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
                                        , Attr.title (vSepLabel vIdx model.cols ++ " row " ++ String.fromInt (toInt r + 1) ++ ": " ++ lineStyleLabel effectiveStyle)
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
                                    [ Attr.id ("insert-row-" ++ indexToString r)
                                    , Attr.class "pill-add-btn"
                                    , Attr.title ("Insert row before row " ++ String.fromInt (toInt r + 1))
                                    , onClick (InsertRow r)
                                    ]
                                    [ text "+" ]
                                , button
                                    [ Attr.id ("del-row-" ++ indexToString r)
                                    , Attr.class "pill-del-btn"
                                    , Attr.title ("Remove row " ++ String.fromInt (toInt r + 1))
                                    , onClick (RemoveRow r)
                                    , Attr.disabled (not canDeleteRow)
                                    ]
                                    [ text "×" ]
                                ]
                            ]
                       ]
                )

        bodyRows =
            List.concatMap
                (\r -> [ hSepRow (hLineAbove r), dataRow r ])
                rowRange
                ++ [ hSepRow (lastHLine model.rows), colPillRow ]

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
            , tbody [] bodyRows
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
        ]


viewMarkdownOutput : Model -> Html FrontendMsg
viewMarkdownOutput model =
    let
        collapsed =
            SeqSet.member MarkdownSection model.collapsedSections

        markdown =
            generateMarkdown model.outputFormat model.rows model.cols model.cells model.headerAlignments model.bodyAlignments
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
                        , Attr.rows (max 4 (countToInt model.rows + 2))
                        ]
                        []
                    ]
               )
        )


viewRenderedTable : Model -> Html FrontendMsg
viewRenderedTable model =
    let
        colRange =
            rangeCount model.cols

        cellAttrs r c =
            let
                align =
                    if toInt r == 0 then
                        getHeaderAlignment c model.headerAlignments

                    else
                        getBodyAlignment c model.bodyAlignments

                top =
                    getEffectiveHStyle (hLineAbove r) c model.cellHorizontalStyles model.horizontalLineStyles

                bottom =
                    getEffectiveHStyle (hLineBelow r) c model.cellHorizontalStyles model.horizontalLineStyles

                left =
                    getEffectiveVStyle r (vLineLeft c) model.cellVerticalStyles model.verticalLineStyles

                right =
                    getEffectiveVStyle r (vLineRight c) model.cellVerticalStyles model.verticalLineStyles

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
                        th (cellAttrs (index 0) c) [ text (getCell (index 0) c model.cells) ]
                    )
                    colRange
                )

        bodyRows =
            List.map
                (\r ->
                    tr []
                        (List.map
                            (\c ->
                                td (cellAttrs r c) [ text (getCell r c model.cells) ]
                            )
                            colRange
                        )
                )
                (rangeFrom (index 1) (countMinusOne model.rows))

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
                            , tbody [] bodyRows
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
            generateHtmlTable model.rows model.cols model.cells model.headerAlignments model.bodyAlignments model.horizontalLineStyles model.verticalLineStyles model.cellHorizontalStyles model.cellVerticalStyles
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
                        , Attr.rows (max 4 (countToInt model.rows + 6))
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
            generateBoxDrawing model.rows model.cols model.cells model.headerAlignments model.bodyAlignments model.horizontalLineStyles model.verticalLineStyles model.cellHorizontalStyles model.cellVerticalStyles
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
                        , Attr.rows (max 4 (countToInt model.rows * 2 + 1))
                        ]
                        []
                    ]
               )
        )
