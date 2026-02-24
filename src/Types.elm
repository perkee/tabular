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
    , outputFormat : OutputFormat
    }


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
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
