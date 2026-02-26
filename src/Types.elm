module Types exposing (..)

import Browser
import Effect.Browser.Navigation
import Index exposing (..)
import SeqSet exposing (SeqSet)
import Url exposing (Url)


type alias TableSnapshot =
    { rows : RowCount
    , cols : ColumnCount
    , cells : IndexDict2 Row_ Column_ String
    , headerAlignments : IndexDict Column_ Alignment
    , bodyAlignments : IndexDict Column_ Alignment
    , horizontalLineStyles : IndexDict HLine_ LineStyle
    , verticalLineStyles : IndexDict VLine_ LineStyle
    , cellHorizontalStyles : IndexDict2 HLine_ Column_ LineStyle
    , cellVerticalStyles : IndexDict2 Row_ VLine_ LineStyle
    }


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , rows : RowCount
    , cols : ColumnCount
    , cells : IndexDict2 Row_ Column_ String
    , headerAlignments : IndexDict Column_ Alignment
    , bodyAlignments : IndexDict Column_ Alignment
    , horizontalLineStyles : IndexDict HLine_ LineStyle
    , verticalLineStyles : IndexDict VLine_ LineStyle
    , cellHorizontalStyles : IndexDict2 HLine_ Column_ LineStyle
    , cellVerticalStyles : IndexDict2 Row_ VLine_ LineStyle
    , outputFormat : OutputFormat
    , showImport : Bool
    , importText : String
    , collapsedSections : SeqSet OutputSection
    , undoStack : List TableSnapshot
    }


type Alignment
    = AlignLeft
    | AlignCenter
    | AlignRight


type LineStyle
    = None
    | Thin
    | Thick
    | ThinTripleDash
    | ThickTripleDash
    | ThinQuadDash
    | ThickQuadDash
    | ThinDoubleDash
    | ThickDoubleDash
    | Double


type LineWeight
    = WNone
    | WLight
    | WHeavy
    | WDouble


type OutputFormat
    = Compact
    | Expanded


type OutputSection
    = BoxDrawingSection
    | MarkdownSection
    | PreviewSection
    | HtmlSection


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | CellChanged RowIndex ColumnIndex String
    | AddRow
    | AddColumn
    | RemoveRow RowIndex
    | RemoveColumn ColumnIndex
    | InsertRow RowIndex
    | InsertColumn ColumnIndex
    | SetOutputFormat OutputFormat
    | SetHeaderAlignment ColumnIndex Alignment
    | SetBodyAlignment ColumnIndex Alignment
    | ToggleImport
    | ImportTextChanged String
    | ImportData
    | CycleHorizontalLineStyle HLineIndex
    | CycleVerticalLineStyle VLineIndex
    | CycleCellHorizontalStyle HLineIndex ColumnIndex
    | CycleCellVerticalStyle RowIndex VLineIndex
    | ToggleSection OutputSection
    | Undo


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
