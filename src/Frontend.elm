module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Lamdera
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
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
      , alignments = Dict.empty
      , outputFormat = Expanded
      , showImport = False
      , importText = ""
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                External url ->
                    ( model, Nav.load url )

        UrlChanged _ ->
            ( model, Cmd.none )

        CellChanged row col value ->
            ( { model | cells = Dict.insert ( row, col ) value model.cells }
            , Cmd.none
            )

        AddRow ->
            ( { model | rows = model.rows + 1 }, Cmd.none )

        AddColumn ->
            ( { model | cols = model.cols + 1 }, Cmd.none )

        RemoveRow rowIndex ->
            if model.rows > 1 then
                ( { model
                    | rows = model.rows - 1
                    , cells = removeRow rowIndex model.cells
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        RemoveColumn colIndex ->
            if model.cols > 1 then
                ( { model
                    | cols = model.cols - 1
                    , cells = removeColumn colIndex model.cells
                    , alignments = removeColumnAlignments colIndex model.alignments
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        SetOutputFormat format ->
            ( { model | outputFormat = format }, Cmd.none )

        SetAlignment col alignment ->
            ( { model | alignments = Dict.insert col alignment model.alignments }
            , Cmd.none
            )

        ToggleImport ->
            ( { model | showImport = not model.showImport, importText = "" }
            , Cmd.none
            )

        ImportTextChanged value ->
            ( { model | importText = value }, Cmd.none )

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
                    , alignments = Dict.empty
                    , showImport = False
                    , importText = ""
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )



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


getAlignment : Int -> Dict Int Alignment -> Alignment
getAlignment col alignments =
    Dict.get col alignments |> Maybe.withDefault AlignLeft


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


generateMarkdown : OutputFormat -> Int -> Int -> Dict ( Int, Int ) String -> Dict Int Alignment -> String
generateMarkdown format rows cols cells alignments =
    if rows == 0 || cols == 0 then
        ""

    else
        let
            colRange =
                List.range 0 (cols - 1)

            rowRange =
                List.range 0 (rows - 1)

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
                    cellTexts =
                        case format of
                            Compact ->
                                List.map
                                    (\c -> escapePipe (getCell r c cells))
                                    colRange

                            Expanded ->
                                List.map2
                                    (\c w ->
                                        padContent (getAlignment c alignments)
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
                            (\c w -> separatorCell (getAlignment c alignments) w)
                            colRange
                            colWidths
                            |> String.join " | "
                       )
                    ++ " |"

            headerRow =
                formatRow 0

            dataRows =
                List.map formatRow (List.range 1 (rows - 1))
        in
        String.join "\n" (headerRow :: separatorRow :: dataRows)



-- BOX DRAWING GENERATION


generateBoxDrawing : Int -> Int -> Dict ( Int, Int ) String -> Dict Int Alignment -> String
generateBoxDrawing rows cols cells alignments =
    if rows == 0 || cols == 0 then
        ""

    else
        let
            colRange =
                List.range 0 (cols - 1)

            rowRange =
                List.range 0 (rows - 1)

            colWidths =
                List.map
                    (\c ->
                        List.foldl
                            (\r maxW -> max maxW (String.length (getCell r c cells)))
                            1
                            rowRange
                    )
                    colRange

            horizontalLine left mid right =
                left
                    ++ (List.map (\w -> String.repeat (w + 2) "\u{2500}") colWidths
                            |> String.join mid
                       )
                    ++ right

            topBorder =
                horizontalLine "\u{250C}" "\u{252C}" "\u{2510}"

            midBorder =
                horizontalLine "\u{251C}" "\u{253C}" "\u{2524}"

            bottomBorder =
                horizontalLine "\u{2514}" "\u{2534}" "\u{2518}"

            formatRow r =
                "\u{2502} "
                    ++ (List.map2
                            (\c w ->
                                padContent (getAlignment c alignments)
                                    (getCell r c cells)
                                    w
                            )
                            colRange
                            colWidths
                            |> String.join " \u{2502} "
                       )
                    ++ " \u{2502}"

            allRows =
                List.concatMap
                    (\r ->
                        if r == 0 then
                            [ topBorder, formatRow r ]

                        else
                            [ midBorder, formatRow r ]
                    )
                    rowRange
                    ++ [ bottomBorder ]
        in
        String.join "\n" allRows



-- VIEW


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Tabular"
    , body =
        [ globalStyles
        , div [ Attr.class "app" ]
            [ viewHeader
            , viewTableEditor model
            , viewMarkdownOutput model
            , viewBoxDrawingOutput model
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

.del-col-btn, .del-row-btn {
    background: none;
    border: 1px solid transparent;
    color: #c0c4cc;
    cursor: pointer;
    font-size: 16px;
    width: 28px;
    height: 28px;
    border-radius: 6px;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    transition: all 0.15s;
    line-height: 1;
}

.del-col-btn:hover, .del-row-btn:hover {
    color: #ef4444;
    background: #fef2f2;
    border-color: #fecaca;
}

.del-col-btn:disabled, .del-row-btn:disabled {
    opacity: 0.25;
    cursor: not-allowed;
}

.del-col-btn:disabled:hover, .del-row-btn:disabled:hover {
    color: #c0c4cc;
    background: none;
    border-color: transparent;
}

.align-group {
    display: inline-flex;
    border: 1px solid #e5e7eb;
    border-radius: 6px;
    overflow: hidden;
}

.align-btn {
    padding: 3px 8px;
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

.add-btn:hover {
    background: #f0f4ff;
    border-color: #4a90d9;
    color: #4a90d9;
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

        rowRange =
            List.range 0 (model.rows - 1)

        canDeleteCol =
            model.cols > 1

        canDeleteRow =
            model.rows > 1

        deleteColHeaderRow =
            tr []
                (td [] []
                    :: List.map
                        (\c ->
                            td [ Attr.style "text-align" "center" ]
                                [ button
                                    [ Attr.class "del-col-btn"
                                    , Attr.title ("Remove column " ++ String.fromInt (c + 1))
                                    , onClick (RemoveColumn c)
                                    , Attr.disabled (not canDeleteCol)
                                    ]
                                    [ text "\u{00D7}" ]
                                ]
                        )
                        colRange
                    ++ [ td [] [] ]
                )

        alignmentRow =
            tr []
                (td [] []
                    :: List.map
                        (\c ->
                            let
                                currentAlign =
                                    getAlignment c model.alignments

                                alignBtn align label =
                                    button
                                        [ Attr.class
                                            (if currentAlign == align then
                                                "align-btn active"

                                             else
                                                "align-btn"
                                            )
                                        , onClick (SetAlignment c align)
                                        , Attr.title label
                                        ]
                                        [ text label ]
                            in
                            td [ Attr.style "text-align" "center" ]
                                [ div [ Attr.class "align-group" ]
                                    [ alignBtn AlignLeft "L"
                                    , alignBtn AlignCenter "C"
                                    , alignBtn AlignRight "R"
                                    ]
                                ]
                        )
                        colRange
                    ++ [ td [] [] ]
                )

        dataRow r =
            tr []
                (td [] []
                    :: List.map
                        (\c ->
                            td []
                                [ input
                                    [ Attr.class
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
                        )
                        colRange
                    ++ [ td [ Attr.style "vertical-align" "middle" ]
                            [ button
                                [ Attr.class "del-row-btn"
                                , Attr.title ("Remove row " ++ String.fromInt (r + 1))
                                , onClick (RemoveRow r)
                                , Attr.disabled (not canDeleteRow)
                                ]
                                [ text "\u{00D7}" ]
                            ]
                       ]
                )
    in
    div [ Attr.class "table-container" ]
        ([ if model.showImport then
            div [ Attr.class "import-section" ]
                [ textarea
                    [ Attr.class "import-textarea"
                    , Attr.placeholder "Paste from Excel, Google Sheets, or CSV..."
                    , Attr.value model.importText
                    , onInput ImportTextChanged
                    , Attr.rows 6
                    ]
                    []
                , div [ Attr.class "import-actions" ]
                    [ button
                        [ Attr.class "add-btn"
                        , onClick ImportData
                        , Attr.disabled (String.isEmpty (String.trim model.importText))
                        ]
                        [ text "Import" ]
                    , button [ Attr.class "add-btn", onClick ToggleImport ]
                        [ text "Cancel" ]
                    ]
                ]

           else
            text ""
         , table [ Attr.class "editor-table" ]
            [ thead [] [ deleteColHeaderRow, alignmentRow ]
            , tbody [] (List.map dataRow rowRange)
            ]
         , div [ Attr.class "button-row" ]
            [ button [ Attr.class "add-btn", onClick AddRow ]
                [ text "+ Row" ]
            , button [ Attr.class "add-btn", onClick AddColumn ]
                [ text "+ Column" ]
            , button [ Attr.class "add-btn", onClick ToggleImport ]
                [ text
                    (if model.showImport then
                        "Hide Import"

                     else
                        "Import Data"
                    )
                ]
            ]
         ]
        )


viewMarkdownOutput : Model -> Html FrontendMsg
viewMarkdownOutput model =
    let
        markdown =
            generateMarkdown model.outputFormat model.rows model.cols model.cells model.alignments
    in
    div [ Attr.class "output-section" ]
        [ div [ Attr.class "output-header" ]
            [ span [ Attr.class "output-title" ] [ text "Markdown" ]
            , div [ Attr.class "output-controls" ]
                [ button
                    [ Attr.class
                        (if model.outputFormat == Compact then
                            "format-btn active"

                         else
                            "format-btn"
                        )
                    , onClick (SetOutputFormat Compact)
                    ]
                    [ text "Compact" ]
                , button
                    [ Attr.class
                        (if model.outputFormat == Expanded then
                            "format-btn active"

                         else
                            "format-btn"
                        )
                    , onClick (SetOutputFormat Expanded)
                    ]
                    [ text "Expanded" ]
                , button
                    [ Attr.class "copy-btn"
                    , Attr.attribute "onclick"
                        "var btn=this;navigator.clipboard.writeText(document.getElementById('md-output').value).then(function(){btn.textContent='Copied!';setTimeout(function(){btn.textContent='Copy'},1500)})"
                    ]
                    [ text "Copy" ]
                ]
            ]
        , textarea
            [ Attr.class "output-textarea"
            , Attr.id "md-output"
            , Attr.readonly True
            , Attr.value markdown
            , Attr.rows (max 4 (model.rows + 2))
            ]
            []
        ]


viewBoxDrawingOutput : Model -> Html FrontendMsg
viewBoxDrawingOutput model =
    let
        boxDrawing =
            generateBoxDrawing model.rows model.cols model.cells model.alignments
    in
    div [ Attr.class "output-section" ]
        [ div [ Attr.class "output-header" ]
            [ span [ Attr.class "output-title" ] [ text "Box Drawing" ]
            , button
                [ Attr.class "copy-btn"
                , Attr.attribute "onclick"
                    "var btn=this;navigator.clipboard.writeText(document.getElementById('box-output').value).then(function(){btn.textContent='Copied!';setTimeout(function(){btn.textContent='Copy'},1500)})"
                ]
                [ text "Copy" ]
            ]
        , textarea
            [ Attr.class "output-textarea"
            , Attr.id "box-output"
            , Attr.readonly True
            , Attr.value boxDrawing
            , Attr.rows (max 4 (model.rows * 2 + 1))
            ]
            []
        ]
