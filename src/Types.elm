module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , rows : Int
    , cols : Int
    , cells : Dict ( Int, Int ) String
    , alignments : Dict Int Alignment
    , horizontalLineStyles : Dict Int LineStyle
    , verticalLineStyles : Dict Int LineStyle
    , cellHorizontalStyles : Dict ( Int, Int ) LineStyle
    , cellVerticalStyles : Dict ( Int, Int ) LineStyle
    , outputFormat : OutputFormat
    , showImport : Bool
    , importText : String
    }


type Alignment
    = AlignLeft
    | AlignCenter
    | AlignRight


type LineStyle
    = Thin
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


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | CellChanged Int Int String
    | AddRow
    | AddColumn
    | RemoveRow Int
    | RemoveColumn Int
    | SetOutputFormat OutputFormat
    | SetAlignment Int Alignment
    | ToggleImport
    | ImportTextChanged String
    | ImportData
    | CycleHorizontalLineStyle Int
    | CycleVerticalLineStyle Int
    | CycleCellHorizontalStyle Int Int
    | CycleCellVerticalStyle Int Int
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
