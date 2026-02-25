module Evergreen.V2.Types exposing (..)

import Browser
import Dict
import Effect.Browser.Navigation
import SeqSet
import Url


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


type OutputFormat
    = Compact
    | Expanded


type OutputSection
    = BoxDrawingSection
    | MarkdownSection
    | PreviewSection
    | HtmlSection


type alias FrontendModel =
    { key : Effect.Browser.Navigation.Key
    , rows : Int
    , cols : Int
    , cells : Dict.Dict ( Int, Int ) String
    , alignments : Dict.Dict Int Alignment
    , horizontalLineStyles : Dict.Dict Int LineStyle
    , verticalLineStyles : Dict.Dict Int LineStyle
    , cellHorizontalStyles : Dict.Dict ( Int, Int ) LineStyle
    , cellVerticalStyles : Dict.Dict ( Int, Int ) LineStyle
    , outputFormat : OutputFormat
    , showImport : Bool
    , importText : String
    , collapsedSections : SeqSet.SeqSet OutputSection
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CellChanged Int Int String
    | AddRow
    | AddColumn
    | RemoveRow Int
    | RemoveColumn Int
    | InsertRow Int
    | InsertColumn Int
    | SetOutputFormat OutputFormat
    | SetAlignment Int Alignment
    | ToggleImport
    | ImportTextChanged String
    | ImportData
    | CycleHorizontalLineStyle Int
    | CycleVerticalLineStyle Int
    | CycleCellHorizontalStyle Int Int
    | CycleCellVerticalStyle Int Int
    | ToggleSection OutputSection


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
