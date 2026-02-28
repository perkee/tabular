module UnitTests exposing (..)

import Dict
import Expect
import Frontend exposing (..)
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
                    [ \r -> Expect.equal 2 r.rows
                    , \r -> Expect.equal 3 r.cols
                    , \r -> Expect.equal (Just "a") (Dict.get ( 0, 0 ) r.cells)
                    , \r -> Expect.equal (Just "b") (Dict.get ( 0, 1 ) r.cells)
                    , \r -> Expect.equal (Just "c") (Dict.get ( 0, 2 ) r.cells)
                    , \r -> Expect.equal (Just "1") (Dict.get ( 1, 0 ) r.cells)
                    , \r -> Expect.equal (Just "2") (Dict.get ( 1, 1 ) r.cells)
                    , \r -> Expect.equal (Just "3") (Dict.get ( 1, 2 ) r.cells)
                    ]
                    result
        , test "TSV input parses correctly" <|
            \_ ->
                let
                    result =
                        parseImportData "x\ty\n1\t2"
                in
                Expect.all
                    [ \r -> Expect.equal 2 r.rows
                    , \r -> Expect.equal 2 r.cols
                    , \r -> Expect.equal (Just "x") (Dict.get ( 0, 0 ) r.cells)
                    , \r -> Expect.equal (Just "y") (Dict.get ( 0, 1 ) r.cells)
                    , \r -> Expect.equal (Just "1") (Dict.get ( 1, 0 ) r.cells)
                    , \r -> Expect.equal (Just "2") (Dict.get ( 1, 1 ) r.cells)
                    ]
                    result
        , test "empty input returns 0 rows and 0 cols" <|
            \_ ->
                let
                    result =
                        parseImportData ""
                in
                Expect.all
                    [ \r -> Expect.equal 0 r.rows
                    , \r -> Expect.equal 0 r.cols
                    ]
                    result
        , test "whitespace-only input returns 0 rows and 0 cols" <|
            \_ ->
                let
                    result =
                        parseImportData "   \n  "
                in
                Expect.all
                    [ \r -> Expect.equal 0 r.rows
                    , \r -> Expect.equal 0 r.cols
                    ]
                    result
        , test "single cell" <|
            \_ ->
                let
                    result =
                        parseImportData "hello"
                in
                Expect.all
                    [ \r -> Expect.equal 1 r.rows
                    , \r -> Expect.equal 1 r.cols
                    , \r -> Expect.equal (Just "hello") (Dict.get ( 0, 0 ) r.cells)
                    ]
                    result
        , test "ragged rows uses max column count" <|
            \_ ->
                let
                    result =
                        parseImportData "a,b,c\n1,2"
                in
                Expect.all
                    [ \r -> Expect.equal 2 r.rows
                    , \r -> Expect.equal 3 r.cols
                    , \r -> Expect.equal Nothing (Dict.get ( 1, 2 ) r.cells)
                    ]
                    result
        , test "trims cell values" <|
            \_ ->
                let
                    result =
                        parseImportData " a , b \n c , d "
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (Dict.get ( 0, 0 ) r.cells)
                    , \r -> Expect.equal (Just "b") (Dict.get ( 0, 1 ) r.cells)
                    , \r -> Expect.equal (Just "c") (Dict.get ( 1, 0 ) r.cells)
                    , \r -> Expect.equal (Just "d") (Dict.get ( 1, 1 ) r.cells)
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
                        Dict.fromList
                            [ ( ( 0, 0 ), "H1" )
                            , ( ( 0, 1 ), "H2" )
                            , ( ( 1, 0 ), "a" )
                            , ( ( 1, 1 ), "b" )
                            ]

                    result =
                        generateMarkdown Expanded 2 2 cells Dict.empty Dict.empty [ 1 ] []
                in
                Expect.equal "| H1  | H2  |\n| --- | --- |\n| a   | b   |" result
        , test "compact format produces minimal separators for left alignment" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "H1" )
                            , ( ( 0, 1 ), "H2" )
                            , ( ( 1, 0 ), "a" )
                            , ( ( 1, 1 ), "b" )
                            ]

                    result =
                        generateMarkdown Compact 2 2 cells Dict.empty Dict.empty [ 1 ] []
                in
                Expect.equal "| H1 | H2 |\n| --- | --- |\n| a | b |" result
        , test "compact format with center alignment produces :-:" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "H" )
                            , ( ( 1, 0 ), "x" )
                            ]

                    bodyAlignments =
                        Dict.fromList [ ( 0, AlignCenter ) ]

                    result =
                        generateMarkdown Compact 2 1 cells Dict.empty bodyAlignments [ 1 ] []
                in
                Expect.equal "| H |\n| :-: |\n| x |" result
        , test "compact format with right alignment produces --:" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "H" )
                            , ( ( 1, 0 ), "x" )
                            ]

                    bodyAlignments =
                        Dict.fromList [ ( 0, AlignRight ) ]

                    result =
                        generateMarkdown Compact 2 1 cells Dict.empty bodyAlignments [ 1 ] []
                in
                Expect.equal "| H |\n| --: |\n| x |" result
        , test "expanded format with center alignment" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "Head" )
                            , ( ( 1, 0 ), "data" )
                            ]

                    bodyAlignments =
                        Dict.fromList [ ( 0, AlignCenter ) ]

                    result =
                        generateMarkdown Expanded 2 1 cells Dict.empty bodyAlignments [ 1 ] []
                in
                Expect.equal "| Head |\n| :--: |\n| data |" result
        , test "expanded format with right alignment" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "Head" )
                            , ( ( 1, 0 ), "data" )
                            ]

                    bodyAlignments =
                        Dict.fromList [ ( 0, AlignRight ) ]

                    result =
                        generateMarkdown Expanded 2 1 cells Dict.empty bodyAlignments [ 1 ] []
                in
                Expect.equal "| Head |\n| ---: |\n| data |" result
        , test "pipe characters in cells are escaped" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "H" )
                            , ( ( 1, 0 ), "a|b" )
                            ]

                    result =
                        generateMarkdown Compact 2 1 cells Dict.empty Dict.empty [ 1 ] []
                in
                Expect.equal True
                    (String.contains "a\\|b" result)
        , test "empty table (0 rows) returns empty string" <|
            \_ ->
                Expect.equal "" (generateMarkdown Expanded 0 2 Dict.empty Dict.empty Dict.empty [] [])
        , test "empty table (0 cols) returns empty string" <|
            \_ ->
                Expect.equal "" (generateMarkdown Expanded 2 0 Dict.empty Dict.empty Dict.empty [ 1 ] [])
        , test "separator uses body alignment, header row uses header alignment" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "Head" )
                            , ( ( 1, 0 ), "data" )
                            ]

                    headerAlignments =
                        Dict.fromList [ ( 0, AlignCenter ) ]

                    bodyAlignments =
                        Dict.fromList [ ( 0, AlignRight ) ]

                    result =
                        generateMarkdown Expanded 2 1 cells headerAlignments bodyAlignments [ 1 ] []
                in
                -- Header row padded center, separator right-aligned, body row padded right
                Expect.equal "| Head |\n| ---: |\n| data |" result
        , test "single row table (header only)" <|
            \_ ->
                let
                    cells =
                        Dict.fromList [ ( ( 0, 0 ), "H1" ), ( ( 0, 1 ), "H2" ) ]

                    result =
                        generateMarkdown Expanded 1 2 cells Dict.empty Dict.empty [] []
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
                        Dict.fromList [ ( ( 0, 0 ), "A" ) ]

                    result =
                        generateBoxDrawing 1 1 cells Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty [] [] Dict.empty
                in
                Expect.equal "┌───┐\n│ A │\n└───┘" result
        , test "basic 2x2 table with Thin borders" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "A" )
                            , ( ( 0, 1 ), "B" )
                            , ( ( 1, 0 ), "C" )
                            , ( ( 1, 1 ), "D" )
                            ]

                    result =
                        generateBoxDrawing 2 2 cells Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty [ 1 ] [] Dict.empty
                in
                Expect.equal "┌───┬───┐\n│ A │ B │\n├───┼───┤\n│ C │ D │\n└───┴───┘" result
        , test "empty table returns empty string" <|
            \_ ->
                Expect.equal "" (generateBoxDrawing 0 1 Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty [] [] Dict.empty)
        , test "alignment affects cell padding" <|
            \_ ->
                let
                    cells =
                        Dict.fromList [ ( ( 0, 0 ), "AB" ) ]

                    alignRight =
                        Dict.fromList [ ( 0, AlignRight ) ]

                    result =
                        generateBoxDrawing 1 1 cells alignRight alignRight Dict.empty Dict.empty Dict.empty Dict.empty [] [] Dict.empty
                in
                -- Right aligned: "AB" should be right-padded
                Expect.equal True
                    (String.contains "AB" result)
        , test "Double line style produces correct characters" <|
            \_ ->
                let
                    cells =
                        Dict.fromList [ ( ( 0, 0 ), "X" ) ]

                    hStyles =
                        Dict.fromList [ ( 0, Double ), ( 1, Double ) ]

                    vStyles =
                        Dict.fromList [ ( 0, Double ), ( 1, Double ) ]

                    result =
                        generateBoxDrawing 1 1 cells Dict.empty Dict.empty hStyles vStyles Dict.empty Dict.empty [] [] Dict.empty
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
                        Dict.fromList [ ( ( 0, 0 ), "X" ) ]

                    hStyles =
                        Dict.fromList [ ( 0, Thick ), ( 1, Thick ) ]

                    vStyles =
                        Dict.fromList [ ( 0, Thick ), ( 1, Thick ) ]

                    result =
                        generateBoxDrawing 1 1 cells Dict.empty Dict.empty hStyles vStyles Dict.empty Dict.empty [] [] Dict.empty
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
                        Dict.fromList
                            [ ( ( 0, 0 ), "H1" )
                            , ( ( 1, 0 ), "D1" )
                            ]

                    result =
                        generateHtmlTable 2 1 cells Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty [ 1 ] []
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
                        Dict.fromList
                            [ ( ( 0, 0 ), "H" )
                            , ( ( 1, 0 ), "<b>bold&\"test\"</b>" )
                            ]

                    result =
                        generateHtmlTable 2 1 cells Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty [ 1 ] []
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
                        Dict.fromList
                            [ ( ( 0, 0 ), "H" )
                            , ( ( 1, 0 ), "D" )
                            ]

                    bodyAlignments =
                        Dict.fromList [ ( 0, AlignCenter ) ]

                    result =
                        generateHtmlTable 2 1 cells Dict.empty bodyAlignments Dict.empty Dict.empty Dict.empty Dict.empty [ 1 ] []
                in
                Expect.equal True
                    (String.contains "text-align: center" result)
        , test "custom border styles generate inline CSS" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "H" )
                            , ( ( 1, 0 ), "D" )
                            ]

                    hStyles =
                        Dict.fromList [ ( 0, Thick ) ]

                    result =
                        generateHtmlTable 2 1 cells Dict.empty Dict.empty hStyles Dict.empty Dict.empty Dict.empty [ 1 ] []
                in
                Expect.equal True
                    (String.contains "border-top: 3px solid" result)
        , test "single-row table has no tbody" <|
            \_ ->
                let
                    cells =
                        Dict.fromList [ ( ( 0, 0 ), "H" ) ]

                    result =
                        generateHtmlTable 1 1 cells Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty [] []
                in
                Expect.all
                    [ \r -> Expect.equal True (String.contains "<thead>" r)
                    , \r -> Expect.equal False (String.contains "<tbody>" r)
                    ]
                    result
        , test "empty table returns empty string" <|
            \_ ->
                Expect.equal "" (generateHtmlTable 0 1 Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty [] [])
        , test "right alignment in HTML" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "H" )
                            , ( ( 1, 0 ), "D" )
                            ]

                    bodyAlignments =
                        Dict.fromList [ ( 0, AlignRight ) ]

                    result =
                        generateHtmlTable 2 1 cells Dict.empty bodyAlignments Dict.empty Dict.empty Dict.empty Dict.empty [ 1 ] []
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
                        Dict.fromList
                            [ ( ( 0, 0 ), "a" )
                            , ( ( 1, 0 ), "b" )
                            , ( ( 2, 0 ), "c" )
                            ]

                    result =
                        removeRow 1 cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (Dict.get ( 0, 0 ) r)
                    , \r -> Expect.equal (Just "c") (Dict.get ( 1, 0 ) r)
                    , \r -> Expect.equal Nothing (Dict.get ( 2, 0 ) r)
                    , \r -> Expect.equal 2 (Dict.size r)
                    ]
                    result
        , test "removes first row" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "a" )
                            , ( ( 1, 0 ), "b" )
                            ]

                    result =
                        removeRow 0 cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "b") (Dict.get ( 0, 0 ) r)
                    , \r -> Expect.equal 1 (Dict.size r)
                    ]
                    result
        , test "removes last row" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "a" )
                            , ( ( 1, 0 ), "b" )
                            ]

                    result =
                        removeRow 1 cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (Dict.get ( 0, 0 ) r)
                    , \r -> Expect.equal 1 (Dict.size r)
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
                        Dict.fromList
                            [ ( ( 0, 0 ), "a" )
                            , ( ( 0, 1 ), "b" )
                            , ( ( 0, 2 ), "c" )
                            ]

                    result =
                        removeColumn 1 cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (Dict.get ( 0, 0 ) r)
                    , \r -> Expect.equal (Just "c") (Dict.get ( 0, 1 ) r)
                    , \r -> Expect.equal Nothing (Dict.get ( 0, 2 ) r)
                    , \r -> Expect.equal 2 (Dict.size r)
                    ]
                    result
        , test "removes first column" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "a" )
                            , ( ( 0, 1 ), "b" )
                            ]

                    result =
                        removeColumn 0 cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "b") (Dict.get ( 0, 0 ) r)
                    , \r -> Expect.equal 1 (Dict.size r)
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
                        Dict.fromList
                            [ ( 0, AlignLeft )
                            , ( 1, AlignCenter )
                            , ( 2, AlignRight )
                            ]

                    result =
                        removeColumnAlignments 1 alignments
                in
                Expect.all
                    [ \r -> Expect.equal (Just AlignLeft) (Dict.get 0 r)
                    , \r -> Expect.equal (Just AlignRight) (Dict.get 1 r)
                    , \r -> Expect.equal Nothing (Dict.get 2 r)
                    , \r -> Expect.equal 2 (Dict.size r)
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
                        Dict.fromList
                            [ ( ( 0, 0 ), Thin )
                            , ( ( 1, 0 ), Thick )
                            , ( ( 2, 0 ), Double )
                            , ( ( 3, 0 ), Thin )
                            ]

                    result =
                        removeCellStyleRow 1 styles
                in
                Expect.all
                    [ \r -> Expect.equal (Just Thin) (Dict.get ( 0, 0 ) r)
                    , \r -> Expect.equal Nothing (Dict.get ( 1, 0 ) r)
                    , \r -> Expect.equal (Just Thin) (Dict.get ( 2, 0 ) r)
                    , \r -> Expect.equal 2 (Dict.size r)
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
                        Dict.fromList
                            [ ( ( 0, 0 ), Thin )
                            , ( ( 0, 1 ), Thick )
                            , ( ( 0, 2 ), Double )
                            ]

                    result =
                        removeCellStyleCol 1 styles
                in
                Expect.all
                    [ \r -> Expect.equal (Just Thin) (Dict.get ( 0, 0 ) r)
                    , \r -> Expect.equal (Just Double) (Dict.get ( 0, 1 ) r)
                    , \r -> Expect.equal 2 (Dict.size r)
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
                        Dict.fromList
                            [ ( ( 0, 0 ), "a" )
                            , ( ( 1, 0 ), "b" )
                            ]

                    result =
                        insertRow 0 cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (Dict.get ( 1, 0 ) r)
                    , \r -> Expect.equal (Just "b") (Dict.get ( 2, 0 ) r)
                    , \r -> Expect.equal Nothing (Dict.get ( 0, 0 ) r)
                    , \r -> Expect.equal 2 (Dict.size r)
                    ]
                    result
        , test "inserts in the middle" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "a" )
                            , ( ( 1, 0 ), "b" )
                            , ( ( 2, 0 ), "c" )
                            ]

                    result =
                        insertRow 1 cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (Dict.get ( 0, 0 ) r)
                    , \r -> Expect.equal (Just "b") (Dict.get ( 2, 0 ) r)
                    , \r -> Expect.equal (Just "c") (Dict.get ( 3, 0 ) r)
                    , \r -> Expect.equal Nothing (Dict.get ( 1, 0 ) r)
                    , \r -> Expect.equal 3 (Dict.size r)
                    ]
                    result
        , test "inserts at the end" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "a" )
                            , ( ( 1, 0 ), "b" )
                            ]

                    result =
                        insertRow 2 cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (Dict.get ( 0, 0 ) r)
                    , \r -> Expect.equal (Just "b") (Dict.get ( 1, 0 ) r)
                    , \r -> Expect.equal 2 (Dict.size r)
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
                        Dict.fromList
                            [ ( ( 0, 0 ), "a" )
                            , ( ( 0, 1 ), "b" )
                            ]

                    result =
                        insertColumn 0 cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (Dict.get ( 0, 1 ) r)
                    , \r -> Expect.equal (Just "b") (Dict.get ( 0, 2 ) r)
                    , \r -> Expect.equal Nothing (Dict.get ( 0, 0 ) r)
                    , \r -> Expect.equal 2 (Dict.size r)
                    ]
                    result
        , test "inserts in the middle" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "a" )
                            , ( ( 0, 1 ), "b" )
                            , ( ( 0, 2 ), "c" )
                            ]

                    result =
                        insertColumn 1 cells
                in
                Expect.all
                    [ \r -> Expect.equal (Just "a") (Dict.get ( 0, 0 ) r)
                    , \r -> Expect.equal (Just "b") (Dict.get ( 0, 2 ) r)
                    , \r -> Expect.equal (Just "c") (Dict.get ( 0, 3 ) r)
                    , \r -> Expect.equal Nothing (Dict.get ( 0, 1 ) r)
                    , \r -> Expect.equal 3 (Dict.size r)
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
                        Dict.fromList
                            [ ( 0, AlignLeft )
                            , ( 1, AlignCenter )
                            , ( 2, AlignRight )
                            ]

                    result =
                        insertColumnAlignments 1 alignments
                in
                Expect.all
                    [ \r -> Expect.equal (Just AlignLeft) (Dict.get 0 r)
                    , \r -> Expect.equal Nothing (Dict.get 1 r)
                    , \r -> Expect.equal (Just AlignCenter) (Dict.get 2 r)
                    , \r -> Expect.equal (Just AlignRight) (Dict.get 3 r)
                    , \r -> Expect.equal 3 (Dict.size r)
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
                        Dict.fromList
                            [ ( 0, Thin )
                            , ( 1, Thick )
                            , ( 2, Double )
                            ]

                    result =
                        insertIndexIntoDict 1 dict
                in
                Expect.all
                    [ \r -> Expect.equal (Just Thin) (Dict.get 0 r)
                    , \r -> Expect.equal Nothing (Dict.get 1 r)
                    , \r -> Expect.equal (Just Thick) (Dict.get 2 r)
                    , \r -> Expect.equal (Just Double) (Dict.get 3 r)
                    , \r -> Expect.equal 3 (Dict.size r)
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
                        Dict.fromList
                            [ ( ( 0, 0 ), Thin )
                            , ( ( 1, 0 ), Thick )
                            , ( ( 2, 0 ), Double )
                            ]

                    result =
                        insertCellStyleRow 1 styles
                in
                Expect.all
                    [ \r -> Expect.equal (Just Thin) (Dict.get ( 0, 0 ) r)
                    , \r -> Expect.equal (Just Thick) (Dict.get ( 1, 0 ) r)
                    , \r -> Expect.equal (Just Double) (Dict.get ( 3, 0 ) r)
                    , \r -> Expect.equal 3 (Dict.size r)
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
                        Dict.fromList
                            [ ( ( 0, 0 ), Thin )
                            , ( ( 0, 1 ), Thick )
                            , ( ( 0, 2 ), Double )
                            ]

                    result =
                        insertCellStyleCol 1 styles
                in
                Expect.all
                    [ \r -> Expect.equal (Just Thin) (Dict.get ( 0, 0 ) r)
                    , \r -> Expect.equal (Just Thick) (Dict.get ( 0, 2 ) r)
                    , \r -> Expect.equal (Just Double) (Dict.get ( 0, 3 ) r)
                    , \r -> Expect.equal 3 (Dict.size r)
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
                        { rows = 2
                        , cols = 3
                        , cells = Dict.fromList [ ( ( 0, 0 ), "A" ) ]
                        , headerAlignments = Dict.fromList [ ( 0, AlignCenter ) ]
                        , bodyAlignments = Dict.fromList [ ( 0, AlignRight ) ]
                        , horizontalLineStyles = Dict.fromList [ ( 0, Thick ) ]
                        , verticalLineStyles = Dict.fromList [ ( 1, Double ) ]
                        , cellHorizontalStyles = Dict.empty
                        , cellVerticalStyles = Dict.empty
                        }
                in
                Expect.all
                    [ \snap -> Expect.equal 2 snap.rows
                    , \snap -> Expect.equal 3 snap.cols
                    , \snap -> Expect.equal (Just "A") (Dict.get ( 0, 0 ) snap.cells)
                    , \snap -> Expect.equal (Just AlignCenter) (Dict.get 0 snap.headerAlignments)
                    , \snap -> Expect.equal (Just AlignRight) (Dict.get 0 snap.bodyAlignments)
                    , \snap -> Expect.equal (Just Thick) (Dict.get 0 snap.horizontalLineStyles)
                    , \snap -> Expect.equal (Just Double) (Dict.get 1 snap.verticalLineStyles)
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
                        Dict.fromList [ ( ( 0, 0 ), Thick ) ]

                    rowStyles =
                        Dict.fromList [ ( 0, Thin ) ]
                in
                Expect.equal Thick (getEffectiveHStyle 0 0 cellStyles rowStyles)
        , test "falls back to row style when no cell override" <|
            \_ ->
                let
                    rowStyles =
                        Dict.fromList [ ( 0, Double ) ]
                in
                Expect.equal Double (getEffectiveHStyle 0 0 Dict.empty rowStyles)
        , test "defaults to Thin when no style set" <|
            \_ ->
                Expect.equal Thin (getEffectiveHStyle 0 0 Dict.empty Dict.empty)
        , test "vertical: cell override takes precedence" <|
            \_ ->
                let
                    cellStyles =
                        Dict.fromList [ ( ( 0, 1 ), Double ) ]

                    colStyles =
                        Dict.fromList [ ( 1, Thick ) ]
                in
                Expect.equal Double (getEffectiveVStyle 0 1 cellStyles colStyles)
        , test "vertical: falls back to column style" <|
            \_ ->
                let
                    colStyles =
                        Dict.fromList [ ( 1, Thick ) ]
                in
                Expect.equal Thick (getEffectiveVStyle 0 1 Dict.empty colStyles)
        ]


hSepLabelTests : Test
hSepLabelTests =
    describe "hSepLabel"
        [ test "index 0 is Top border" <|
            \_ ->
                Expect.equal "Top border" (hSepLabel 0 3 False)
        , test "index == rows is Bottom border without summary" <|
            \_ ->
                Expect.equal "Bottom border" (hSepLabel 3 3 False)
        , test "index == rows is Above summary with summary" <|
            \_ ->
                Expect.equal "Above summary" (hSepLabel 3 3 True)
        , test "index > rows is Bottom border with summary" <|
            \_ ->
                Expect.equal "Bottom border" (hSepLabel 4 3 True)
        , test "middle index shows row range" <|
            \_ ->
                Expect.equal "Row 1-2" (hSepLabel 1 3 False)
        , test "another middle index" <|
            \_ ->
                Expect.equal "Row 2-3" (hSepLabel 2 3 False)
        ]


vSepLabelTests : Test
vSepLabelTests =
    describe "vSepLabel"
        [ test "index 0 is Left border" <|
            \_ ->
                Expect.equal "Left border" (vSepLabel 0 3)
        , test "index == cols is Right border" <|
            \_ ->
                Expect.equal "Right border" (vSepLabel 3 3)
        , test "middle index shows column range" <|
            \_ ->
                Expect.equal "Col 1-2" (vSepLabel 1 3)
        ]



-- SORTING


extractNumericTests : Test
extractNumericTests =
    describe "extractNumeric"
        [ test "plain integer" <|
            \_ ->
                Expect.equal (Just 42) (extractNumeric "42")
        , test "plain float" <|
            \_ ->
                Expect.equal (Just 3.14) (extractNumeric "3.14")
        , test "currency prefix" <|
            \_ ->
                Expect.equal (Just 19.99) (extractNumeric "$19.99")
        , test "no digits returns Nothing" <|
            \_ ->
                Expect.equal Nothing (extractNumeric "abc")
        , test "empty string returns Nothing" <|
            \_ ->
                Expect.equal Nothing (extractNumeric "")
        , test "mixed text and digits" <|
            \_ ->
                Expect.equal (Just 100) (extractNumeric "about 100 items")
        , test "negative sign stripped (digits only)" <|
            \_ ->
                Expect.equal (Just 5) (extractNumeric "-5")
        ]


computeSortedBodyRowsTests : Test
computeSortedBodyRowsTests =
    describe "computeSortedBodyRows"
        [ test "Unsorted returns insertion order" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "H" )
                            , ( ( 1, 0 ), "c" )
                            , ( ( 2, 0 ), "a" )
                            , ( ( 3, 0 ), "b" )
                            ]
                in
                Expect.equal [ 1, 2, 3 ] (computeSortedBodyRows 4 cells Unsorted)
        , test "lexicographic ascending" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "H" )
                            , ( ( 1, 0 ), "cherry" )
                            , ( ( 2, 0 ), "apple" )
                            , ( ( 3, 0 ), "banana" )
                            ]
                in
                Expect.equal [ 2, 3, 1 ] (computeSortedBodyRows 4 cells (SortedBy 0 Ascending Lexicographic))
        , test "lexicographic descending" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "H" )
                            , ( ( 1, 0 ), "cherry" )
                            , ( ( 2, 0 ), "apple" )
                            , ( ( 3, 0 ), "banana" )
                            ]
                in
                Expect.equal [ 1, 3, 2 ] (computeSortedBodyRows 4 cells (SortedBy 0 Descending Lexicographic))
        , test "numeric ascending" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "H" )
                            , ( ( 1, 0 ), "10" )
                            , ( ( 2, 0 ), "2" )
                            , ( ( 3, 0 ), "100" )
                            ]
                in
                Expect.equal [ 2, 1, 3 ] (computeSortedBodyRows 4 cells (SortedBy 0 Ascending Numeric))
        , test "numeric descending" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "H" )
                            , ( ( 1, 0 ), "10" )
                            , ( ( 2, 0 ), "2" )
                            , ( ( 3, 0 ), "100" )
                            ]
                in
                Expect.equal [ 3, 1, 2 ] (computeSortedBodyRows 4 cells (SortedBy 0 Descending Numeric))
        , test "numeric sort: non-numeric cells go to bottom in ascending" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "H" )
                            , ( ( 1, 0 ), "N/A" )
                            , ( ( 2, 0 ), "5" )
                            , ( ( 3, 0 ), "3" )
                            ]
                in
                Expect.equal [ 3, 2, 1 ] (computeSortedBodyRows 4 cells (SortedBy 0 Ascending Numeric))
        , test "numeric sort: non-numeric cells go to bottom in descending" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "H" )
                            , ( ( 1, 0 ), "N/A" )
                            , ( ( 2, 0 ), "5" )
                            , ( ( 3, 0 ), "3" )
                            ]
                in
                Expect.equal [ 2, 3, 1 ] (computeSortedBodyRows 4 cells (SortedBy 0 Descending Numeric))
        , test "sorts by specified column" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "Name" )
                            , ( ( 0, 1 ), "Score" )
                            , ( ( 1, 0 ), "Alice" )
                            , ( ( 1, 1 ), "90" )
                            , ( ( 2, 0 ), "Bob" )
                            , ( ( 2, 1 ), "80" )
                            , ( ( 3, 0 ), "Carol" )
                            , ( ( 3, 1 ), "95" )
                            ]
                in
                Expect.equal [ 2, 1, 3 ] (computeSortedBodyRows 4 cells (SortedBy 1 Ascending Numeric))
        ]


sortedMarkdownTests : Test
sortedMarkdownTests =
    describe "generateMarkdown with sorted body rows"
        [ test "sorted body rows reorder output" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "H" )
                            , ( ( 1, 0 ), "b" )
                            , ( ( 2, 0 ), "a" )
                            ]

                    result =
                        generateMarkdown Compact 3 1 cells Dict.empty Dict.empty [ 2, 1 ] []
                in
                Expect.equal "| H |\n| --- |\n| a |\n| b |" result
        ]



-- SUMMARY ROWS


computeSummaryRowTests : Test
computeSummaryRowTests =
    describe "computeSummaryRow"
        [ test "SummaryMax with numeric data" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "Name" )
                            , ( ( 0, 1 ), "Score" )
                            , ( ( 1, 0 ), "Alice" )
                            , ( ( 1, 1 ), "10" )
                            , ( ( 2, 0 ), "Bob" )
                            , ( ( 2, 1 ), "20" )
                            ]

                    result =
                        computeSummaryRow SummaryMax 2 cells [ 1, 2 ]
                in
                Expect.equal [ "MAX", "20" ] result
        , test "SummaryMax with mixed data" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "Name" )
                            , ( ( 0, 1 ), "Value" )
                            , ( ( 1, 0 ), "Alice" )
                            , ( ( 1, 1 ), "5" )
                            , ( ( 2, 0 ), "Bob" )
                            , ( ( 2, 1 ), "N/A" )
                            ]

                    result =
                        computeSummaryRow SummaryMax 2 cells [ 1, 2 ]
                in
                Expect.equal [ "MAX", "5" ] result
        , test "SummaryMax with all non-numeric" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "Name" )
                            , ( ( 0, 1 ), "Notes" )
                            , ( ( 1, 0 ), "Alice" )
                            , ( ( 1, 1 ), "good" )
                            ]

                    result =
                        computeSummaryRow SummaryMax 2 cells [ 1 ]
                in
                Expect.equal [ "MAX", "" ] result
        , test "SummaryMax with float values" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "Name" )
                            , ( ( 0, 1 ), "Value" )
                            , ( ( 1, 0 ), "A" )
                            , ( ( 1, 1 ), "3.14" )
                            , ( ( 2, 0 ), "B" )
                            , ( ( 2, 1 ), "2.71" )
                            ]

                    result =
                        computeSummaryRow SummaryMax 2 cells [ 1, 2 ]
                in
                Expect.equal [ "MAX", "3.14" ] result
        , test "SummaryMin with numeric data" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "Name" )
                            , ( ( 0, 1 ), "Score" )
                            , ( ( 1, 0 ), "Alice" )
                            , ( ( 1, 1 ), "10" )
                            , ( ( 2, 0 ), "Bob" )
                            , ( ( 2, 1 ), "20" )
                            ]

                    result =
                        computeSummaryRow SummaryMin 2 cells [ 1, 2 ]
                in
                Expect.equal [ "MIN", "10" ] result
        , test "SummaryMin with mixed data" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "Name" )
                            , ( ( 0, 1 ), "Value" )
                            , ( ( 1, 0 ), "Alice" )
                            , ( ( 1, 1 ), "5" )
                            , ( ( 2, 0 ), "Bob" )
                            , ( ( 2, 1 ), "N/A" )
                            ]

                    result =
                        computeSummaryRow SummaryMin 2 cells [ 1, 2 ]
                in
                Expect.equal [ "MIN", "5" ] result
        , test "SummaryMin with all non-numeric" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "Name" )
                            , ( ( 0, 1 ), "Notes" )
                            , ( ( 1, 0 ), "Alice" )
                            , ( ( 1, 1 ), "good" )
                            ]

                    result =
                        computeSummaryRow SummaryMin 2 cells [ 1 ]
                in
                Expect.equal [ "MIN", "" ] result
        , test "SummaryMin with float values" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "Name" )
                            , ( ( 0, 1 ), "Value" )
                            , ( ( 1, 0 ), "A" )
                            , ( ( 1, 1 ), "3.14" )
                            , ( ( 2, 0 ), "B" )
                            , ( ( 2, 1 ), "2.71" )
                            ]

                    result =
                        computeSummaryRow SummaryMin 2 cells [ 1, 2 ]
                in
                Expect.equal [ "MIN", "2.71" ] result
        ]


formatSummaryValueTests : Test
formatSummaryValueTests =
    describe "formatSummaryValue"
        [ test "integer value has no decimal" <|
            \_ ->
                Expect.equal "42" (formatSummaryValue 42)
        , test "float value keeps decimal" <|
            \_ ->
                Expect.equal "3.14" (formatSummaryValue 3.14)
        , test "zero" <|
            \_ ->
                Expect.equal "0" (formatSummaryValue 0)
        ]


summaryInMarkdownTests : Test
summaryInMarkdownTests =
    describe "generateMarkdown with summary rows"
        [ test "MAX row appended with bold values" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "Name" )
                            , ( ( 0, 1 ), "Val" )
                            , ( ( 1, 0 ), "A" )
                            , ( ( 1, 1 ), "10" )
                            , ( ( 2, 0 ), "B" )
                            , ( ( 2, 1 ), "20" )
                            ]

                    result =
                        generateMarkdown Compact 3 2 cells Dict.empty Dict.empty [ 1, 2 ] [ SummaryMax ]
                in
                Expect.equal True (String.contains "**MAX**" result && String.contains "**20**" result)
        , test "MIN row appended with bold values" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "Name" )
                            , ( ( 0, 1 ), "Val" )
                            , ( ( 1, 0 ), "A" )
                            , ( ( 1, 1 ), "10" )
                            , ( ( 2, 0 ), "B" )
                            , ( ( 2, 1 ), "20" )
                            ]

                    result =
                        generateMarkdown Compact 3 2 cells Dict.empty Dict.empty [ 1, 2 ] [ SummaryMin ]
                in
                Expect.equal True (String.contains "**MIN**" result && String.contains "**10**" result)
        ]


summaryInHtmlTests : Test
summaryInHtmlTests =
    describe "generateHtmlTable with summary rows"
        [ test "tfoot with MAX row" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "Name" )
                            , ( ( 0, 1 ), "Val" )
                            , ( ( 1, 0 ), "A" )
                            , ( ( 1, 1 ), "10" )
                            ]

                    result =
                        generateHtmlTable 2 2 cells Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty [ 1 ] [ SummaryMax ]
                in
                Expect.all
                    [ \r -> Expect.equal True (String.contains "<tfoot>" r)
                    , \r -> Expect.equal True (String.contains "<th scope=\"row\">MAX</th>" r)
                    , \r -> Expect.equal True (String.contains "<td>10</td>" r)
                    , \r -> Expect.equal True (String.contains "</tfoot>" r)
                    ]
                    result
        , test "tfoot with MIN row" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "Name" )
                            , ( ( 0, 1 ), "Val" )
                            , ( ( 1, 0 ), "A" )
                            , ( ( 1, 1 ), "10" )
                            ]

                    result =
                        generateHtmlTable 2 2 cells Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty [ 1 ] [ SummaryMin ]
                in
                Expect.all
                    [ \r -> Expect.equal True (String.contains "<tfoot>" r)
                    , \r -> Expect.equal True (String.contains "<th scope=\"row\">MIN</th>" r)
                    , \r -> Expect.equal True (String.contains "<td>10</td>" r)
                    , \r -> Expect.equal True (String.contains "</tfoot>" r)
                    ]
                    result
        ]


summaryInBoxDrawingTests : Test
summaryInBoxDrawingTests =
    describe "generateBoxDrawing with summary rows"
        [ test "MAX row with thin separator before bottom border" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "N" )
                            , ( ( 0, 1 ), "V" )
                            , ( ( 1, 0 ), "A" )
                            , ( ( 1, 1 ), "5" )
                            , ( ( 2, 0 ), "B" )
                            , ( ( 2, 1 ), "9" )
                            ]

                    result =
                        generateBoxDrawing 3 2 cells Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty [ 1, 2 ] [ SummaryMax ] Dict.empty
                in
                Expect.all
                    [ \r -> Expect.equal True (String.contains "MAX" r)
                    , \r -> Expect.equal True (String.contains "9" r)
                    ]
                    result
        , test "MIN row with thin separator before bottom border" <|
            \_ ->
                let
                    cells =
                        Dict.fromList
                            [ ( ( 0, 0 ), "N" )
                            , ( ( 0, 1 ), "V" )
                            , ( ( 1, 0 ), "A" )
                            , ( ( 1, 1 ), "5" )
                            , ( ( 2, 0 ), "B" )
                            , ( ( 2, 1 ), "9" )
                            ]

                    result =
                        generateBoxDrawing 3 2 cells Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty Dict.empty [ 1, 2 ] [ SummaryMin ] Dict.empty
                in
                Expect.all
                    [ \r -> Expect.equal True (String.contains "MIN" r)
                    , \r -> Expect.equal True (String.contains "5" r)
                    ]
                    result
        ]
