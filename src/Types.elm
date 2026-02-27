module Types exposing (..)

import Browser
import Dict exposing (Dict)
import Effect.Browser.Navigation
import SeqSet exposing (SeqSet)
import Url exposing (Url)


type alias TableSnapshot =
    { rows : Int
    , cols : Int
    , cells : Dict ( Int, Int ) String
    , headerAlignments : Dict Int Alignment
    , bodyAlignments : Dict Int Alignment
    , horizontalLineStyles : Dict Int LineStyle
    , verticalLineStyles : Dict Int LineStyle
    , cellHorizontalStyles : Dict ( Int, Int ) LineStyle
    , cellVerticalStyles : Dict ( Int, Int ) LineStyle
    }


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , rows : Int
    , cols : Int
    , cells : Dict ( Int, Int ) String
    , headerAlignments : Dict Int Alignment
    , bodyAlignments : Dict Int Alignment
    , horizontalLineStyles : Dict Int LineStyle
    , verticalLineStyles : Dict Int LineStyle
    , cellHorizontalStyles : Dict ( Int, Int ) LineStyle
    , cellVerticalStyles : Dict ( Int, Int ) LineStyle
    , outputFormat : OutputFormat
    , showImport : Bool
    , importText : String
    , collapsedSections : SeqSet OutputSection
    , undoStack : List TableSnapshot
    , sortState : SortState
    , summaryRows : SeqSet SummaryFunction
    , summarySeparatorStyles : Dict Int LineStyle
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


type SortDirection
    = Ascending
    | Descending


type SortMethod
    = Lexicographic
    | Numeric


type SortState
    = Unsorted
    | SortedBy Int SortDirection SortMethod


type SummaryFunction
    = SummaryMax


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url
    | CellChanged Int Int String
    | AddRow
    | AddColumn
    | RemoveRow Int
    | RemoveColumn Int
    | InsertRow Int
    | InsertColumn Int
    | SetOutputFormat OutputFormat
    | SetHeaderAlignment Int Alignment
    | SetBodyAlignment Int Alignment
    | ToggleImport
    | ImportTextChanged String
    | ImportData
    | CycleHorizontalLineStyle Int
    | CycleVerticalLineStyle Int
    | CycleCellHorizontalStyle Int Int
    | CycleCellVerticalStyle Int Int
    | ToggleSection OutputSection
    | Undo
    | SetSortColumn String
    | SetSortDirection SortDirection
    | SetSortMethod SortMethod
    | ApplySortToInputs
    | ToggleSummaryRow SummaryFunction
    | CycleSummarySeparatorStyle Int


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
