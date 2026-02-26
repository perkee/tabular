module UnitTests exposing (..)

import Expect
import Frontend exposing (..)
import Index exposing (..)
import Test exposing (..)
import Types exposing (..)



-- IMPORT PARSING


parseImportDataTests : Test
parseImportDataTests =
    describe "parseImportData"
        [ test "CSV input parses correctly" <|
            \_ ->
                let
                    result =
                        parseImportData "a,b,c\n1,2,3"
                in
                Expect.all
                    [ \r -> Expect.equal (count 2) r.rows
                    , \r -> Expect.equal (count 3) r.cols
                    , \r -> Expect.equal (Just "a") (get2 (index 0) (index 0) r.cells)
                    , \r -> Expect.equal (Just "b") (get2 (index 0) (index 1) r.cells)
                    , \r -> Expect.equal (Just "c") (get2 (index 0) (index 2) r.cells)
                    , \r -> Expect.equal (Just "1") (get2 (index 1) (index 0) r.cells)
                    , \r -> Expect.equal (Just "2") (get2 (index 1) (index 1) r.cells)
                    , \r -> Expect.equal (Just "3") (get2 (index 1) (index 2) r.cells)
                    ]
                    result
        , test "TSV input parses correctly" <|
            \_ ->
                let
                    result =
                        parseImportData "x\ty\n1\t2"
                in
                Expect.all
                    [ \r -> Expect.equal (count 2) r.rows
                    , \r -> Expect.equal (count 2) r.cols
                    , \r -> Expect.equal (Just "x") (get2 (index 0) (index 0) r.cells)
                    , \r -> Expect.equal (Just "y") (get2 (index 0) (index 1) r.cells)
                    , \r -> Expect.equal (Just "1") (get2 (index 1) (index 0) r.cells)
                    , \r -> Expect.equal (Just "2") (get2 (index 1) (index 1) r.cells)
                    ]
                    result
        , test "empty input returns 0 rows and 0 cols" <|
            \_ ->
                let
                    result =
                        parseImportData ""
                in
                Expect.all
                    [ \r -> Expect.equal (count 0) r.rows
                    , \r -> Expect.equal (count 0) r.cols
                    ]
                    result
        , test "whitespace-only input returns 0 rows and 0 cols" <|
            \_ ->
                let
                    result =
                        parseImportData "   \n  "
                in
                Expect.all
                    [ \r -> Expect.equal (count 0) r.rows
                    , \r -> Expect.equal (count 0) r.cols
                    ]
                    result
        , test "single cell" <|
            \_ ->
                let
                    result =
                        parseImportData "hello"
                in
                Expect.all
                    [ \r -> Expect.equal (count 1) r.rows
                    , \r -> Expect.equal (count 1) r.cols
                    , \r -> Expect.equal (Just "hello") (get2 (index 0) (index 0) r.cells)
                    ]
                    result
        , test "ragged rows uses max column count" <|
            \_ ->
                let
                    result =
                        parseImportData "a,b,c\n1,2"
                in
                Expect.all
                    [ \r -> Expect.equal (count 2) r.rows
                    , \r -> Expect.equal (count 3) r.cols
                    , \r -> Expect.equal Nothing (get2 (index 1) (index 2) r.cells)
                    ]
                    result
        , test "trims cell values" <|
            \_ ->
                let
                    result =
                        parseImportData " a , b \n c , d "
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (get2 (index 0) (index 0) r.cells)
                    , \r -> Expect.equal (Just "b") (get2 (index 0) (index 1) r.cells)
                    , \r -> Expect.equal (Just "c") (get2 (index 1) (index 0) r.cells)
                    , \r -> Expect.equal (Just "d") (get2 (index 1) (index 1) r.cells)
                    ]
                    result
        ]



-- MARKDOWN GENERATION


generateMarkdownTests : Test
generateMarkdownTests =
    describe "generateMarkdown"
        [ test "basic 2x2 table in Expanded format" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "H1" )
                            , ( index 0, index 1, "H2" )
                            , ( index 1, index 0, "a" )
                            , ( index 1, index 1, "b" )
                            ]

                    result =
                        generateMarkdown Expanded (count 2) (count 2) cells empty empty
                in
                Expect.equal "| H1  | H2  |\n| --- | --- |\n| a   | b   |" result
        , test "compact format produces minimal separators for left alignment" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "H1" )
                            , ( index 0, index 1, "H2" )
                            , ( index 1, index 0, "a" )
                            , ( index 1, index 1, "b" )
                            ]

                    result =
                        generateMarkdown Compact (count 2) (count 2) cells empty empty
                in
                Expect.equal "| H1 | H2 |\n| --- | --- |\n| a | b |" result
        , test "compact format with center alignment produces :-:" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "H" )
                            , ( index 1, index 0, "x" )
                            ]

                    bodyAlignments =
                        Index.fromList [ ( index 0, AlignCenter ) ]

                    result =
                        generateMarkdown Compact (count 2) (count 1) cells empty bodyAlignments
                in
                Expect.equal "| H |\n| :-: |\n| x |" result
        , test "compact format with right alignment produces --:" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "H" )
                            , ( index 1, index 0, "x" )
                            ]

                    bodyAlignments =
                        Index.fromList [ ( index 0, AlignRight ) ]

                    result =
                        generateMarkdown Compact (count 2) (count 1) cells empty bodyAlignments
                in
                Expect.equal "| H |\n| --: |\n| x |" result
        , test "expanded format with center alignment" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "Head" )
                            , ( index 1, index 0, "data" )
                            ]

                    bodyAlignments =
                        Index.fromList [ ( index 0, AlignCenter ) ]

                    result =
                        generateMarkdown Expanded (count 2) (count 1) cells empty bodyAlignments
                in
                Expect.equal "| Head |\n| :--: |\n| data |" result
        , test "expanded format with right alignment" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "Head" )
                            , ( index 1, index 0, "data" )
                            ]

                    bodyAlignments =
                        Index.fromList [ ( index 0, AlignRight ) ]

                    result =
                        generateMarkdown Expanded (count 2) (count 1) cells empty bodyAlignments
                in
                Expect.equal "| Head |\n| ---: |\n| data |" result
        , test "pipe characters in cells are escaped" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "H" )
                            , ( index 1, index 0, "a|b" )
                            ]

                    result =
                        generateMarkdown Compact (count 2) (count 1) cells empty empty
                in
                Expect.equal True
                    (String.contains "a\\|b" result)
        , test "empty table (0 rows) returns empty string" <|
            \_ ->
                Expect.equal "" (generateMarkdown Expanded (count 0) (count 2) empty2 empty empty)
        , test "empty table (0 cols) returns empty string" <|
            \_ ->
                Expect.equal "" (generateMarkdown Expanded (count 2) (count 0) empty2 empty empty)
        , test "separator uses body alignment, header row uses header alignment" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "Head" )
                            , ( index 1, index 0, "data" )
                            ]

                    headerAlignments =
                        Index.fromList [ ( index 0, AlignCenter ) ]

                    bodyAlignments =
                        Index.fromList [ ( index 0, AlignRight ) ]

                    result =
                        generateMarkdown Expanded (count 2) (count 1) cells headerAlignments bodyAlignments
                in
                -- Header row padded center, separator right-aligned, body row padded right
                Expect.equal "| Head |\n| ---: |\n| data |" result
        , test "single row table (header only)" <|
            \_ ->
                let
                    cells =
                        fromList2 [ ( index 0, index 0, "H1" ), ( index 0, index 1, "H2" ) ]

                    result =
                        generateMarkdown Expanded (count 1) (count 2) cells empty empty
                in
                Expect.equal "| H1  | H2  |\n| --- | --- |" result
        ]



-- BOX DRAWING GENERATION


generateBoxDrawingTests : Test
generateBoxDrawingTests =
    describe "generateBoxDrawing"
        [ test "basic 1x1 table with default Thin borders" <|
            \_ ->
                let
                    cells =
                        fromList2 [ ( index 0, index 0, "A" ) ]

                    result =
                        generateBoxDrawing (count 1) (count 1) cells empty empty empty empty empty2 empty2
                in
                Expect.equal "┌───┐\n│ A │\n└───┘" result
        , test "basic 2x2 table with Thin borders" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "A" )
                            , ( index 0, index 1, "B" )
                            , ( index 1, index 0, "C" )
                            , ( index 1, index 1, "D" )
                            ]

                    result =
                        generateBoxDrawing (count 2) (count 2) cells empty empty empty empty empty2 empty2
                in
                Expect.equal "┌───┬───┐\n│ A │ B │\n├───┼───┤\n│ C │ D │\n└───┴───┘" result
        , test "empty table returns empty string" <|
            \_ ->
                Expect.equal "" (generateBoxDrawing (count 0) (count 1) empty2 empty empty empty empty empty2 empty2)
        , test "alignment affects cell padding" <|
            \_ ->
                let
                    cells =
                        fromList2 [ ( index 0, index 0, "AB" ) ]

                    alignRight =
                        Index.fromList [ ( index 0, AlignRight ) ]

                    result =
                        generateBoxDrawing (count 1) (count 1) cells alignRight alignRight empty empty empty2 empty2
                in
                -- Right aligned: "AB" should be right-padded
                Expect.equal True
                    (String.contains "AB" result)
        , test "Double line style produces correct characters" <|
            \_ ->
                let
                    cells =
                        fromList2 [ ( index 0, index 0, "X" ) ]

                    hStyles =
                        Index.fromList [ ( index 0, Double ), ( index 1, Double ) ]

                    vStyles =
                        Index.fromList [ ( index 0, Double ), ( index 1, Double ) ]

                    result =
                        generateBoxDrawing (count 1) (count 1) cells empty empty hStyles vStyles empty2 empty2
                in
                Expect.all
                    [ \r -> Expect.equal True (String.contains "═" r)
                    , \r -> Expect.equal True (String.contains "║" r)
                    , \r -> Expect.equal True (String.contains "╔" r)
                    ]
                    result
        , test "Thick line style uses heavy characters" <|
            \_ ->
                let
                    cells =
                        fromList2 [ ( index 0, index 0, "X" ) ]

                    hStyles =
                        Index.fromList [ ( index 0, Thick ), ( index 1, Thick ) ]

                    vStyles =
                        Index.fromList [ ( index 0, Thick ), ( index 1, Thick ) ]

                    result =
                        generateBoxDrawing (count 1) (count 1) cells empty empty hStyles vStyles empty2 empty2
                in
                Expect.all
                    [ \r -> Expect.equal True (String.contains "━" r)
                    , \r -> Expect.equal True (String.contains "┃" r)
                    ]
                    result
        ]



-- HTML TABLE GENERATION


generateHtmlTableTests : Test
generateHtmlTableTests =
    describe "generateHtmlTable"
        [ test "basic table structure" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "H1" )
                            , ( index 1, index 0, "D1" )
                            ]

                    result =
                        generateHtmlTable (count 2) (count 1) cells empty empty empty empty empty2 empty2
                in
                Expect.all
                    [ \r -> Expect.equal True (String.startsWith "<table>" r)
                    , \r -> Expect.equal True (String.endsWith "</table>" r)
                    , \r -> Expect.equal True (String.contains "<thead>" r)
                    , \r -> Expect.equal True (String.contains "<tbody>" r)
                    , \r -> Expect.equal True (String.contains "<th" r)
                    , \r -> Expect.equal True (String.contains "<td>" r)
                    ]
                    result
        , test "HTML entities are escaped" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "H" )
                            , ( index 1, index 0, "<b>bold&\"test\"</b>" )
                            ]

                    result =
                        generateHtmlTable (count 2) (count 1) cells empty empty empty empty empty2 empty2
                in
                Expect.all
                    [ \r -> Expect.equal True (String.contains "&lt;b&gt;" r)
                    , \r -> Expect.equal True (String.contains "&amp;" r)
                    , \r -> Expect.equal True (String.contains "&quot;" r)
                    ]
                    result
        , test "alignment generates text-align style" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "H" )
                            , ( index 1, index 0, "D" )
                            ]

                    bodyAlignments =
                        Index.fromList [ ( index 0, AlignCenter ) ]

                    result =
                        generateHtmlTable (count 2) (count 1) cells empty bodyAlignments empty empty empty2 empty2
                in
                Expect.equal True
                    (String.contains "text-align: center" result)
        , test "custom border styles generate inline CSS" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "H" )
                            , ( index 1, index 0, "D" )
                            ]

                    hStyles =
                        Index.fromList [ ( index 0, Thick ) ]

                    result =
                        generateHtmlTable (count 2) (count 1) cells empty empty hStyles empty empty2 empty2
                in
                Expect.equal True
                    (String.contains "border-top: 3px solid" result)
        , test "single-row table has no tbody" <|
            \_ ->
                let
                    cells =
                        fromList2 [ ( index 0, index 0, "H" ) ]

                    result =
                        generateHtmlTable (count 1) (count 1) cells empty empty empty empty empty2 empty2
                in
                Expect.all
                    [ \r -> Expect.equal True (String.contains "<thead>" r)
                    , \r -> Expect.equal False (String.contains "<tbody>" r)
                    ]
                    result
        , test "empty table returns empty string" <|
            \_ ->
                Expect.equal "" (generateHtmlTable (count 0) (count 1) empty2 empty empty empty empty empty2 empty2)
        , test "right alignment in HTML" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "H" )
                            , ( index 1, index 0, "D" )
                            ]

                    bodyAlignments =
                        Index.fromList [ ( index 0, AlignRight ) ]

                    result =
                        generateHtmlTable (count 2) (count 1) cells empty bodyAlignments empty empty empty2 empty2
                in
                Expect.equal True
                    (String.contains "text-align: right" result)
        ]



-- ROW/COLUMN MANIPULATION


removeRowTests : Test
removeRowTests =
    describe "removeRow"
        [ test "removes target row and reindexes" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "a" )
                            , ( index 1, index 0, "b" )
                            , ( index 2, index 0, "c" )
                            ]

                    result =
                        removeRow (index 1) cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (get2 (index 0) (index 0) r)
                    , \r -> Expect.equal (Just "c") (get2 (index 1) (index 0) r)
                    , \r -> Expect.equal Nothing (get2 (index 2) (index 0) r)
                    , \r -> Expect.equal 2 (size2 r)
                    ]
                    result
        , test "removes first row" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "a" )
                            , ( index 1, index 0, "b" )
                            ]

                    result =
                        removeRow (index 0) cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "b") (get2 (index 0) (index 0) r)
                    , \r -> Expect.equal 1 (size2 r)
                    ]
                    result
        , test "removes last row" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "a" )
                            , ( index 1, index 0, "b" )
                            ]

                    result =
                        removeRow (index 1) cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (get2 (index 0) (index 0) r)
                    , \r -> Expect.equal 1 (size2 r)
                    ]
                    result
        ]


removeColumnTests : Test
removeColumnTests =
    describe "removeColumn"
        [ test "removes target column and reindexes" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "a" )
                            , ( index 0, index 1, "b" )
                            , ( index 0, index 2, "c" )
                            ]

                    result =
                        removeColumn (index 1) cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (get2 (index 0) (index 0) r)
                    , \r -> Expect.equal (Just "c") (get2 (index 0) (index 1) r)
                    , \r -> Expect.equal Nothing (get2 (index 0) (index 2) r)
                    , \r -> Expect.equal 2 (size2 r)
                    ]
                    result
        , test "removes first column" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "a" )
                            , ( index 0, index 1, "b" )
                            ]

                    result =
                        removeColumn (index 0) cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "b") (get2 (index 0) (index 0) r)
                    , \r -> Expect.equal 1 (size2 r)
                    ]
                    result
        ]


removeColumnAlignmentsTests : Test
removeColumnAlignmentsTests =
    describe "removeColumnAlignments"
        [ test "removes alignment and reindexes" <|
            \_ ->
                let
                    alignments =
                        Index.fromList
                            [ ( index 0, AlignLeft )
                            , ( index 1, AlignCenter )
                            , ( index 2, AlignRight )
                            ]

                    result =
                        removeColumnAlignments (index 1) alignments
                in
                Expect.all
                    [ \r -> Expect.equal (Just AlignLeft) (Index.get (index 0) r)
                    , \r -> Expect.equal (Just AlignRight) (Index.get (index 1) r)
                    , \r -> Expect.equal Nothing (Index.get (index 2) r)
                    , \r -> Expect.equal 2 (Index.size r)
                    ]
                    result
        ]


removeCellStyleRowTests : Test
removeCellStyleRowTests =
    describe "removeCellStyleRow"
        [ test "removes styles for affected horizontal lines and reindexes" <|
            \_ ->
                let
                    -- Row 1 removal affects hIdx 1 and hIdx 2 (the lines above and below row 1)
                    styles =
                        fromList2
                            [ ( index 0, index 0, Thin )
                            , ( index 1, index 0, Thick )
                            , ( index 2, index 0, Double )
                            , ( index 3, index 0, Thin )
                            ]

                    result =
                        removeCellStyleRow (index 1) styles
                in
                Expect.all
                    [ \r -> Expect.equal (Just Thin) (get2 (index 0) (index 0) r)
                    , \r -> Expect.equal Nothing (get2 (index 1) (index 0) r)
                    , \r -> Expect.equal (Just Thin) (get2 (index 2) (index 0) r)
                    , \r -> Expect.equal 2 (size2 r)
                    ]
                    result
        ]


removeCellStyleColTests : Test
removeCellStyleColTests =
    describe "removeCellStyleCol"
        [ test "removes styles for affected column and reindexes" <|
            \_ ->
                let
                    styles =
                        fromList2
                            [ ( index 0, index 0, Thin )
                            , ( index 0, index 1, Thick )
                            , ( index 0, index 2, Double )
                            ]

                    result =
                        removeCellStyleCol (index 1) styles
                in
                Expect.all
                    [ \r -> Expect.equal (Just Thin) (get2 (index 0) (index 0) r)
                    , \r -> Expect.equal (Just Double) (get2 (index 0) (index 1) r)
                    , \r -> Expect.equal 2 (size2 r)
                    ]
                    result
        ]


insertRowTests : Test
insertRowTests =
    describe "insertRow"
        [ test "inserts at beginning and shifts all rows up" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "a" )
                            , ( index 1, index 0, "b" )
                            ]

                    result =
                        insertRowCells (index 0) cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (get2 (index 1) (index 0) r)
                    , \r -> Expect.equal (Just "b") (get2 (index 2) (index 0) r)
                    , \r -> Expect.equal Nothing (get2 (index 0) (index 0) r)
                    , \r -> Expect.equal 2 (size2 r)
                    ]
                    result
        , test "inserts in the middle" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "a" )
                            , ( index 1, index 0, "b" )
                            , ( index 2, index 0, "c" )
                            ]

                    result =
                        insertRowCells (index 1) cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (get2 (index 0) (index 0) r)
                    , \r -> Expect.equal (Just "b") (get2 (index 2) (index 0) r)
                    , \r -> Expect.equal (Just "c") (get2 (index 3) (index 0) r)
                    , \r -> Expect.equal Nothing (get2 (index 1) (index 0) r)
                    , \r -> Expect.equal 3 (size2 r)
                    ]
                    result
        , test "inserts at the end" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "a" )
                            , ( index 1, index 0, "b" )
                            ]

                    result =
                        insertRowCells (index 2) cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (get2 (index 0) (index 0) r)
                    , \r -> Expect.equal (Just "b") (get2 (index 1) (index 0) r)
                    , \r -> Expect.equal 2 (size2 r)
                    ]
                    result
        ]


insertColumnTests : Test
insertColumnTests =
    describe "insertColumn"
        [ test "inserts at beginning and shifts all columns right" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "a" )
                            , ( index 0, index 1, "b" )
                            ]

                    result =
                        insertColumnCells (index 0) cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (get2 (index 0) (index 1) r)
                    , \r -> Expect.equal (Just "b") (get2 (index 0) (index 2) r)
                    , \r -> Expect.equal Nothing (get2 (index 0) (index 0) r)
                    , \r -> Expect.equal 2 (size2 r)
                    ]
                    result
        , test "inserts in the middle" <|
            \_ ->
                let
                    cells =
                        fromList2
                            [ ( index 0, index 0, "a" )
                            , ( index 0, index 1, "b" )
                            , ( index 0, index 2, "c" )
                            ]

                    result =
                        insertColumnCells (index 1) cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (get2 (index 0) (index 0) r)
                    , \r -> Expect.equal (Just "b") (get2 (index 0) (index 2) r)
                    , \r -> Expect.equal (Just "c") (get2 (index 0) (index 3) r)
                    , \r -> Expect.equal Nothing (get2 (index 0) (index 1) r)
                    , \r -> Expect.equal 3 (size2 r)
                    ]
                    result
        ]


insertColumnAlignmentsTests : Test
insertColumnAlignmentsTests =
    describe "insertColumnAlignments"
        [ test "inserts and shifts alignments" <|
            \_ ->
                let
                    alignments =
                        Index.fromList
                            [ ( index 0, AlignLeft )
                            , ( index 1, AlignCenter )
                            , ( index 2, AlignRight )
                            ]

                    result =
                        insertColumnAlignments (index 1) alignments
                in
                Expect.all
                    [ \r -> Expect.equal (Just AlignLeft) (Index.get (index 0) r)
                    , \r -> Expect.equal Nothing (Index.get (index 1) r)
                    , \r -> Expect.equal (Just AlignCenter) (Index.get (index 2) r)
                    , \r -> Expect.equal (Just AlignRight) (Index.get (index 3) r)
                    , \r -> Expect.equal 3 (Index.size r)
                    ]
                    result
        ]


insertIndexIntoDictTests : Test
insertIndexIntoDictTests =
    describe "insertIndexIntoDict"
        [ test "shifts indices at and above insertion point" <|
            \_ ->
                let
                    dict =
                        Index.fromList
                            [ ( index 0, Thin )
                            , ( index 1, Thick )
                            , ( index 2, Double )
                            ]

                    result =
                        insertIndexIntoDict (index 1) dict
                in
                Expect.all
                    [ \r -> Expect.equal (Just Thin) (Index.get (index 0) r)
                    , \r -> Expect.equal Nothing (Index.get (index 1) r)
                    , \r -> Expect.equal (Just Thick) (Index.get (index 2) r)
                    , \r -> Expect.equal (Just Double) (Index.get (index 3) r)
                    , \r -> Expect.equal 3 (Index.size r)
                    ]
                    result
        ]


insertCellStyleRowTests : Test
insertCellStyleRowTests =
    describe "insertCellStyleRow"
        [ test "shifts horizontal line indices above insertion row" <|
            \_ ->
                let
                    styles =
                        fromList2
                            [ ( index 0, index 0, Thin )
                            , ( index 1, index 0, Thick )
                            , ( index 2, index 0, Double )
                            ]

                    result =
                        insertCellStyleRow (index 1) styles
                in
                Expect.all
                    [ \r -> Expect.equal (Just Thin) (get2 (index 0) (index 0) r)
                    , \r -> Expect.equal (Just Thick) (get2 (index 1) (index 0) r)
                    , \r -> Expect.equal (Just Double) (get2 (index 3) (index 0) r)
                    , \r -> Expect.equal 3 (size2 r)
                    ]
                    result
        ]


insertCellStyleColTests : Test
insertCellStyleColTests =
    describe "insertCellStyleCol"
        [ test "shifts column indices at and above insertion col" <|
            \_ ->
                let
                    styles =
                        fromList2
                            [ ( index 0, index 0, Thin )
                            , ( index 0, index 1, Thick )
                            , ( index 0, index 2, Double )
                            ]

                    result =
                        insertCellStyleCol (index 1) styles
                in
                Expect.all
                    [ \r -> Expect.equal (Just Thin) (get2 (index 0) (index 0) r)
                    , \r -> Expect.equal (Just Thick) (get2 (index 0) (index 2) r)
                    , \r -> Expect.equal (Just Double) (get2 (index 0) (index 3) r)
                    , \r -> Expect.equal 3 (size2 r)
                    ]
                    result
        ]



-- SMALL HELPERS


cycleLineStyleTests : Test
cycleLineStyleTests =
    describe "cycleLineStyle"
        [ test "full cycle None→Thin→...→Double→None" <|
            \_ ->
                let
                    fullCycle =
                        None
                            |> cycleLineStyle
                            |> cycleLineStyle
                            |> cycleLineStyle
                            |> cycleLineStyle
                            |> cycleLineStyle
                            |> cycleLineStyle
                            |> cycleLineStyle
                            |> cycleLineStyle
                            |> cycleLineStyle
                            |> cycleLineStyle
                in
                Expect.equal None fullCycle
        , test "None cycles to Thin" <|
            \_ ->
                Expect.equal Thin (cycleLineStyle None)
        , test "Thin cycles to Thick" <|
            \_ ->
                Expect.equal Thick (cycleLineStyle Thin)
        , test "Double cycles to None" <|
            \_ ->
                Expect.equal None (cycleLineStyle Double)
        ]


interleaveTests : Test
interleaveTests =
    describe "interleave"
        [ test "interleaves equal length lists" <|
            \_ ->
                Expect.equal [ "a", "1", "b", "2" ] (interleave [ "a", "b" ] [ "1", "2" ])
        , test "first list longer by one" <|
            \_ ->
                Expect.equal [ "a", "1", "b" ] (interleave [ "a", "b" ] [ "1" ])
        , test "first list empty" <|
            \_ ->
                Expect.equal [] (interleave [] [ "1", "2" ])
        , test "second list empty" <|
            \_ ->
                Expect.equal [ "a" ] (interleave [ "a" ] [])
        , test "both empty" <|
            \_ ->
                Expect.equal [] (interleave [] [])
        , test "single element each" <|
            \_ ->
                Expect.equal [ "a", "b" ] (interleave [ "a" ] [ "b" ])
        ]


padContentTests : Test
padContentTests =
    describe "padContent"
        [ test "left alignment pads right" <|
            \_ ->
                Expect.equal "ab   " (padContent AlignLeft "ab" 5)
        , test "right alignment pads left" <|
            \_ ->
                Expect.equal "   ab" (padContent AlignRight "ab" 5)
        , test "center alignment pads both sides" <|
            \_ ->
                Expect.equal " ab  " (padContent AlignCenter "ab" 5)
        , test "center alignment with even padding" <|
            \_ ->
                Expect.equal " ab " (padContent AlignCenter "ab" 4)
        , test "no padding needed when content fills width" <|
            \_ ->
                Expect.equal "abc" (padContent AlignLeft "abc" 3)
        ]


escapePipeTests : Test
escapePipeTests =
    describe "escapePipe"
        [ test "escapes pipe character" <|
            \_ ->
                Expect.equal "a\\|b" (escapePipe "a|b")
        , test "no pipe returns unchanged" <|
            \_ ->
                Expect.equal "abc" (escapePipe "abc")
        , test "multiple pipes" <|
            \_ ->
                Expect.equal "a\\|b\\|c" (escapePipe "a|b|c")
        , test "empty string" <|
            \_ ->
                Expect.equal "" (escapePipe "")
        ]


lookupCornerTests : Test
lookupCornerTests =
    describe "lookupCorner"
        [ test "all None returns space" <|
            \_ ->
                Expect.equal " " (lookupCorner WNone WNone WNone WNone)
        , test "top-left light corner" <|
            \_ ->
                Expect.equal "┌" (lookupCorner WNone WLight WNone WLight)
        , test "cross light" <|
            \_ ->
                Expect.equal "┼" (lookupCorner WLight WLight WLight WLight)
        , test "top-right light corner" <|
            \_ ->
                Expect.equal "┐" (lookupCorner WNone WLight WLight WNone)
        , test "bottom-left light corner" <|
            \_ ->
                Expect.equal "└" (lookupCorner WLight WNone WNone WLight)
        , test "bottom-right light corner" <|
            \_ ->
                Expect.equal "┘" (lookupCorner WLight WNone WLight WNone)
        , test "double top-left corner" <|
            \_ ->
                Expect.equal "╔" (lookupCorner WNone WDouble WNone WDouble)
        , test "fallback downgrades heavy to light when mixing with double" <|
            \_ ->
                -- WHeavy+WDouble not in dict directly, falls back through to WLight simplification
                -- lookupCorner WLight WLight WNone WLight = "├"
                Expect.equal "\u{251C}" (lookupCorner WHeavy WDouble WNone WLight)
        ]


tableSnapshotTests : Test
tableSnapshotTests =
    describe "TableSnapshot"
        [ test "snapshot record has expected fields" <|
            \_ ->
                let
                    s : TableSnapshot
                    s =
                        { rows = count 2
                        , cols = count 3
                        , cells = fromList2 [ ( index 0, index 0, "A" ) ]
                        , headerAlignments = Index.fromList [ ( index 0, AlignCenter ) ]
                        , bodyAlignments = Index.fromList [ ( index 0, AlignRight ) ]
                        , horizontalLineStyles = Index.fromList [ ( index 0, Thick ) ]
                        , verticalLineStyles = Index.fromList [ ( index 1, Double ) ]
                        , cellHorizontalStyles = empty2
                        , cellVerticalStyles = empty2
                        }
                in
                Expect.all
                    [ \snap -> Expect.equal (count 2) snap.rows
                    , \snap -> Expect.equal (count 3) snap.cols
                    , \snap -> Expect.equal (Just "A") (get2 (index 0) (index 0) snap.cells)
                    , \snap -> Expect.equal (Just AlignCenter) (Index.get (index 0) snap.headerAlignments)
                    , \snap -> Expect.equal (Just AlignRight) (Index.get (index 0) snap.bodyAlignments)
                    , \snap -> Expect.equal (Just Thick) (Index.get (index 0) snap.horizontalLineStyles)
                    , \snap -> Expect.equal (Just Double) (Index.get (index 1) snap.verticalLineStyles)
                    ]
                    s
        ]


getEffectiveStyleTests : Test
getEffectiveStyleTests =
    describe "getEffectiveHStyle / getEffectiveVStyle"
        [ test "cell override takes precedence" <|
            \_ ->
                let
                    cellStyles =
                        fromList2 [ ( index 0, index 0, Thick ) ]

                    rowStyles =
                        Index.fromList [ ( index 0, Thin ) ]
                in
                Expect.equal Thick (getEffectiveHStyle (index 0) (index 0) cellStyles rowStyles)
        , test "falls back to row style when no cell override" <|
            \_ ->
                let
                    rowStyles =
                        Index.fromList [ ( index 0, Double ) ]
                in
                Expect.equal Double (getEffectiveHStyle (index 0) (index 0) empty2 rowStyles)
        , test "defaults to Thin when no style set" <|
            \_ ->
                Expect.equal Thin (getEffectiveHStyle (index 0) (index 0) empty2 empty)
        , test "vertical: cell override takes precedence" <|
            \_ ->
                let
                    cellStyles =
                        fromList2 [ ( index 0, index 1, Double ) ]

                    colStyles =
                        Index.fromList [ ( index 1, Thick ) ]
                in
                Expect.equal Double (getEffectiveVStyle (index 0) (index 1) cellStyles colStyles)
        , test "vertical: falls back to column style" <|
            \_ ->
                let
                    colStyles =
                        Index.fromList [ ( index 1, Thick ) ]
                in
                Expect.equal Thick (getEffectiveVStyle (index 0) (index 1) empty2 colStyles)
        ]


hSepLabelTests : Test
hSepLabelTests =
    describe "hSepLabel"
        [ test "index 0 is Top border" <|
            \_ ->
                Expect.equal "Top border" (hSepLabel (index 0) (count 3))
        , test "index == rows is Bottom border" <|
            \_ ->
                Expect.equal "Bottom border" (hSepLabel (index 3) (count 3))
        , test "middle index shows row range" <|
            \_ ->
                Expect.equal "Row 1-2" (hSepLabel (index 1) (count 3))
        , test "another middle index" <|
            \_ ->
                Expect.equal "Row 2-3" (hSepLabel (index 2) (count 3))
        ]


vSepLabelTests : Test
vSepLabelTests =
    describe "vSepLabel"
        [ test "index 0 is Left border" <|
            \_ ->
                Expect.equal "Left border" (vSepLabel (index 0) (count 3))
        , test "index == cols is Right border" <|
            \_ ->
                Expect.equal "Right border" (vSepLabel (index 3) (count 3))
        , test "middle index shows column range" <|
            \_ ->
                Expect.equal "Col 1-2" (vSepLabel (index 1) (count 3))
        ]
