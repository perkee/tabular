module Evergreen.Migrate.V5 exposing (..)

import Dict
import Evergreen.V4.Types
import Evergreen.V5.Types
import Lamdera.Migrations exposing (..)
import SeqSet


frontendModel : Evergreen.V4.Types.FrontendModel -> ModelMigration Evergreen.V5.Types.FrontendModel Evergreen.V5.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V4.Types.BackendModel -> ModelMigration Evergreen.V5.Types.BackendModel Evergreen.V5.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V4.Types.FrontendMsg -> MsgMigration Evergreen.V5.Types.FrontendMsg Evergreen.V5.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V4.Types.ToBackend -> MsgMigration Evergreen.V5.Types.ToBackend Evergreen.V5.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V4.Types.BackendMsg -> MsgMigration Evergreen.V5.Types.BackendMsg Evergreen.V5.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V4.Types.ToFrontend -> MsgMigration Evergreen.V5.Types.ToFrontend Evergreen.V5.Types.FrontendMsg
toFrontend old =
    MsgUnchanged


migrate_Types_FrontendModel : Evergreen.V4.Types.FrontendModel -> Evergreen.V5.Types.FrontendModel
migrate_Types_FrontendModel old =
    { key = old.key
    , rows = old.rows
    , cols = old.cols
    , cells = old.cells
    , headerAlignments = old.headerAlignments |> migrate_Dict_Alignment
    , bodyAlignments = old.bodyAlignments |> migrate_Dict_Alignment
    , horizontalLineStyles = old.horizontalLineStyles |> migrate_Dict_LineStyle
    , verticalLineStyles = old.verticalLineStyles |> migrate_Dict_LineStyle
    , cellHorizontalStyles = old.cellHorizontalStyles |> migrate_TupleDict_LineStyle
    , cellVerticalStyles = old.cellVerticalStyles |> migrate_TupleDict_LineStyle
    , outputFormat = old.outputFormat |> migrate_Types_OutputFormat
    , showImport = old.showImport
    , importText = old.importText
    , collapsedSections = old.collapsedSections |> SeqSet.map migrate_Types_OutputSection
    , undoStack = old.undoStack |> List.map migrate_Types_TableSnapshot
    , sortState = Evergreen.V5.Types.Unsorted
    }


migrate_Types_TableSnapshot : Evergreen.V4.Types.TableSnapshot -> Evergreen.V5.Types.TableSnapshot
migrate_Types_TableSnapshot old =
    { rows = old.rows
    , cols = old.cols
    , cells = old.cells
    , headerAlignments = old.headerAlignments |> migrate_Dict_Alignment
    , bodyAlignments = old.bodyAlignments |> migrate_Dict_Alignment
    , horizontalLineStyles = old.horizontalLineStyles |> migrate_Dict_LineStyle
    , verticalLineStyles = old.verticalLineStyles |> migrate_Dict_LineStyle
    , cellHorizontalStyles = old.cellHorizontalStyles |> migrate_TupleDict_LineStyle
    , cellVerticalStyles = old.cellVerticalStyles |> migrate_TupleDict_LineStyle
    }


migrate_Dict_Alignment : Dict.Dict Int Evergreen.V4.Types.Alignment -> Dict.Dict Int Evergreen.V5.Types.Alignment
migrate_Dict_Alignment =
    Dict.map (\_ -> migrate_Types_Alignment)


migrate_Dict_LineStyle : Dict.Dict Int Evergreen.V4.Types.LineStyle -> Dict.Dict Int Evergreen.V5.Types.LineStyle
migrate_Dict_LineStyle =
    Dict.map (\_ -> migrate_Types_LineStyle)


migrate_TupleDict_LineStyle : Dict.Dict ( Int, Int ) Evergreen.V4.Types.LineStyle -> Dict.Dict ( Int, Int ) Evergreen.V5.Types.LineStyle
migrate_TupleDict_LineStyle =
    Dict.map (\_ -> migrate_Types_LineStyle)


migrate_Types_Alignment : Evergreen.V4.Types.Alignment -> Evergreen.V5.Types.Alignment
migrate_Types_Alignment old =
    case old of
        Evergreen.V4.Types.AlignLeft ->
            Evergreen.V5.Types.AlignLeft

        Evergreen.V4.Types.AlignCenter ->
            Evergreen.V5.Types.AlignCenter

        Evergreen.V4.Types.AlignRight ->
            Evergreen.V5.Types.AlignRight


migrate_Types_FrontendMsg : Evergreen.V4.Types.FrontendMsg -> Evergreen.V5.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V4.Types.UrlClicked p0 ->
            Evergreen.V5.Types.UrlClicked p0

        Evergreen.V4.Types.UrlChanged p0 ->
            Evergreen.V5.Types.UrlChanged p0

        Evergreen.V4.Types.CellChanged p0 p1 p2 ->
            Evergreen.V5.Types.CellChanged p0 p1 p2

        Evergreen.V4.Types.AddRow ->
            Evergreen.V5.Types.AddRow

        Evergreen.V4.Types.AddColumn ->
            Evergreen.V5.Types.AddColumn

        Evergreen.V4.Types.RemoveRow p0 ->
            Evergreen.V5.Types.RemoveRow p0

        Evergreen.V4.Types.RemoveColumn p0 ->
            Evergreen.V5.Types.RemoveColumn p0

        Evergreen.V4.Types.InsertRow p0 ->
            Evergreen.V5.Types.InsertRow p0

        Evergreen.V4.Types.InsertColumn p0 ->
            Evergreen.V5.Types.InsertColumn p0

        Evergreen.V4.Types.SetOutputFormat p0 ->
            Evergreen.V5.Types.SetOutputFormat (p0 |> migrate_Types_OutputFormat)

        Evergreen.V4.Types.SetHeaderAlignment p0 p1 ->
            Evergreen.V5.Types.SetHeaderAlignment p0 (p1 |> migrate_Types_Alignment)

        Evergreen.V4.Types.SetBodyAlignment p0 p1 ->
            Evergreen.V5.Types.SetBodyAlignment p0 (p1 |> migrate_Types_Alignment)

        Evergreen.V4.Types.ToggleImport ->
            Evergreen.V5.Types.ToggleImport

        Evergreen.V4.Types.ImportTextChanged p0 ->
            Evergreen.V5.Types.ImportTextChanged p0

        Evergreen.V4.Types.ImportData ->
            Evergreen.V5.Types.ImportData

        Evergreen.V4.Types.CycleHorizontalLineStyle p0 ->
            Evergreen.V5.Types.CycleHorizontalLineStyle p0

        Evergreen.V4.Types.CycleVerticalLineStyle p0 ->
            Evergreen.V5.Types.CycleVerticalLineStyle p0

        Evergreen.V4.Types.CycleCellHorizontalStyle p0 p1 ->
            Evergreen.V5.Types.CycleCellHorizontalStyle p0 p1

        Evergreen.V4.Types.CycleCellVerticalStyle p0 p1 ->
            Evergreen.V5.Types.CycleCellVerticalStyle p0 p1

        Evergreen.V4.Types.ToggleSection p0 ->
            Evergreen.V5.Types.ToggleSection (p0 |> migrate_Types_OutputSection)

        Evergreen.V4.Types.Undo ->
            Evergreen.V5.Types.Undo


migrate_Types_LineStyle : Evergreen.V4.Types.LineStyle -> Evergreen.V5.Types.LineStyle
migrate_Types_LineStyle old =
    case old of
        Evergreen.V4.Types.None ->
            Evergreen.V5.Types.None

        Evergreen.V4.Types.Thin ->
            Evergreen.V5.Types.Thin

        Evergreen.V4.Types.Thick ->
            Evergreen.V5.Types.Thick

        Evergreen.V4.Types.ThinTripleDash ->
            Evergreen.V5.Types.ThinTripleDash

        Evergreen.V4.Types.ThickTripleDash ->
            Evergreen.V5.Types.ThickTripleDash

        Evergreen.V4.Types.ThinQuadDash ->
            Evergreen.V5.Types.ThinQuadDash

        Evergreen.V4.Types.ThickQuadDash ->
            Evergreen.V5.Types.ThickQuadDash

        Evergreen.V4.Types.ThinDoubleDash ->
            Evergreen.V5.Types.ThinDoubleDash

        Evergreen.V4.Types.ThickDoubleDash ->
            Evergreen.V5.Types.ThickDoubleDash

        Evergreen.V4.Types.Double ->
            Evergreen.V5.Types.Double


migrate_Types_OutputFormat : Evergreen.V4.Types.OutputFormat -> Evergreen.V5.Types.OutputFormat
migrate_Types_OutputFormat old =
    case old of
        Evergreen.V4.Types.Compact ->
            Evergreen.V5.Types.Compact

        Evergreen.V4.Types.Expanded ->
            Evergreen.V5.Types.Expanded


migrate_Types_OutputSection : Evergreen.V4.Types.OutputSection -> Evergreen.V5.Types.OutputSection
migrate_Types_OutputSection old =
    case old of
        Evergreen.V4.Types.BoxDrawingSection ->
            Evergreen.V5.Types.BoxDrawingSection

        Evergreen.V4.Types.MarkdownSection ->
            Evergreen.V5.Types.MarkdownSection

        Evergreen.V4.Types.PreviewSection ->
            Evergreen.V5.Types.PreviewSection

        Evergreen.V4.Types.HtmlSection ->
            Evergreen.V5.Types.HtmlSection
