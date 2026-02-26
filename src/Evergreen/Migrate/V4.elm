module Evergreen.Migrate.V4 exposing (..)

import Dict
import Evergreen.V3.Types
import Evergreen.V4.Types
import Lamdera.Migrations exposing (..)
import SeqSet


frontendModel : Evergreen.V3.Types.FrontendModel -> ModelMigration Evergreen.V4.Types.FrontendModel Evergreen.V4.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V3.Types.BackendModel -> ModelMigration Evergreen.V4.Types.BackendModel Evergreen.V4.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V3.Types.FrontendMsg -> MsgMigration Evergreen.V4.Types.FrontendMsg Evergreen.V4.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V3.Types.ToBackend -> MsgMigration Evergreen.V4.Types.ToBackend Evergreen.V4.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V3.Types.BackendMsg -> MsgMigration Evergreen.V4.Types.BackendMsg Evergreen.V4.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V3.Types.ToFrontend -> MsgMigration Evergreen.V4.Types.ToFrontend Evergreen.V4.Types.FrontendMsg
toFrontend old =
    MsgUnchanged


migrate_Types_FrontendModel : Evergreen.V3.Types.FrontendModel -> Evergreen.V4.Types.FrontendModel
migrate_Types_FrontendModel old =
    { key = old.key
    , rows = old.rows
    , cols = old.cols
    , cells = old.cells
    , headerAlignments = Dict.empty
    , bodyAlignments = old.alignments |> Dict.map (\_ -> migrate_Types_Alignment)
    , horizontalLineStyles = old.horizontalLineStyles |> Dict.map (\_ -> migrate_Types_LineStyle)
    , verticalLineStyles = old.verticalLineStyles |> Dict.map (\_ -> migrate_Types_LineStyle)
    , cellHorizontalStyles = old.cellHorizontalStyles |> Dict.map (\_ -> migrate_Types_LineStyle)
    , cellVerticalStyles = old.cellVerticalStyles |> Dict.map (\_ -> migrate_Types_LineStyle)
    , outputFormat = old.outputFormat |> migrate_Types_OutputFormat
    , showImport = old.showImport
    , importText = old.importText
    , collapsedSections = old.collapsedSections |> SeqSet.map migrate_Types_OutputSection
    , undoStack = []
    }


migrate_Types_Alignment : Evergreen.V3.Types.Alignment -> Evergreen.V4.Types.Alignment
migrate_Types_Alignment old =
    case old of
        Evergreen.V3.Types.AlignLeft ->
            Evergreen.V4.Types.AlignLeft

        Evergreen.V3.Types.AlignCenter ->
            Evergreen.V4.Types.AlignCenter

        Evergreen.V3.Types.AlignRight ->
            Evergreen.V4.Types.AlignRight


migrate_Types_FrontendMsg : Evergreen.V3.Types.FrontendMsg -> Evergreen.V4.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V3.Types.UrlClicked p0 ->
            Evergreen.V4.Types.UrlClicked p0

        Evergreen.V3.Types.UrlChanged p0 ->
            Evergreen.V4.Types.UrlChanged p0

        Evergreen.V3.Types.CellChanged p0 p1 p2 ->
            Evergreen.V4.Types.CellChanged p0 p1 p2

        Evergreen.V3.Types.AddRow ->
            Evergreen.V4.Types.AddRow

        Evergreen.V3.Types.AddColumn ->
            Evergreen.V4.Types.AddColumn

        Evergreen.V3.Types.RemoveRow p0 ->
            Evergreen.V4.Types.RemoveRow p0

        Evergreen.V3.Types.RemoveColumn p0 ->
            Evergreen.V4.Types.RemoveColumn p0

        Evergreen.V3.Types.InsertRow p0 ->
            Evergreen.V4.Types.InsertRow p0

        Evergreen.V3.Types.InsertColumn p0 ->
            Evergreen.V4.Types.InsertColumn p0

        Evergreen.V3.Types.SetOutputFormat p0 ->
            Evergreen.V4.Types.SetOutputFormat (p0 |> migrate_Types_OutputFormat)

        Evergreen.V3.Types.SetAlignment p0 p1 ->
            Evergreen.V4.Types.SetBodyAlignment p0 (p1 |> migrate_Types_Alignment)

        Evergreen.V3.Types.ToggleImport ->
            Evergreen.V4.Types.ToggleImport

        Evergreen.V3.Types.ImportTextChanged p0 ->
            Evergreen.V4.Types.ImportTextChanged p0

        Evergreen.V3.Types.ImportData ->
            Evergreen.V4.Types.ImportData

        Evergreen.V3.Types.CycleHorizontalLineStyle p0 ->
            Evergreen.V4.Types.CycleHorizontalLineStyle p0

        Evergreen.V3.Types.CycleVerticalLineStyle p0 ->
            Evergreen.V4.Types.CycleVerticalLineStyle p0

        Evergreen.V3.Types.CycleCellHorizontalStyle p0 p1 ->
            Evergreen.V4.Types.CycleCellHorizontalStyle p0 p1

        Evergreen.V3.Types.CycleCellVerticalStyle p0 p1 ->
            Evergreen.V4.Types.CycleCellVerticalStyle p0 p1

        Evergreen.V3.Types.ToggleSection p0 ->
            Evergreen.V4.Types.ToggleSection (p0 |> migrate_Types_OutputSection)

        Evergreen.V3.Types.Undo ->
            Evergreen.V4.Types.Undo


migrate_Types_LineStyle : Evergreen.V3.Types.LineStyle -> Evergreen.V4.Types.LineStyle
migrate_Types_LineStyle old =
    case old of
        Evergreen.V3.Types.None ->
            Evergreen.V4.Types.None

        Evergreen.V3.Types.Thin ->
            Evergreen.V4.Types.Thin

        Evergreen.V3.Types.Thick ->
            Evergreen.V4.Types.Thick

        Evergreen.V3.Types.ThinTripleDash ->
            Evergreen.V4.Types.ThinTripleDash

        Evergreen.V3.Types.ThickTripleDash ->
            Evergreen.V4.Types.ThickTripleDash

        Evergreen.V3.Types.ThinQuadDash ->
            Evergreen.V4.Types.ThinQuadDash

        Evergreen.V3.Types.ThickQuadDash ->
            Evergreen.V4.Types.ThickQuadDash

        Evergreen.V3.Types.ThinDoubleDash ->
            Evergreen.V4.Types.ThinDoubleDash

        Evergreen.V3.Types.ThickDoubleDash ->
            Evergreen.V4.Types.ThickDoubleDash

        Evergreen.V3.Types.Double ->
            Evergreen.V4.Types.Double


migrate_Types_OutputFormat : Evergreen.V3.Types.OutputFormat -> Evergreen.V4.Types.OutputFormat
migrate_Types_OutputFormat old =
    case old of
        Evergreen.V3.Types.Compact ->
            Evergreen.V4.Types.Compact

        Evergreen.V3.Types.Expanded ->
            Evergreen.V4.Types.Expanded


migrate_Types_OutputSection : Evergreen.V3.Types.OutputSection -> Evergreen.V4.Types.OutputSection
migrate_Types_OutputSection old =
    case old of
        Evergreen.V3.Types.BoxDrawingSection ->
            Evergreen.V4.Types.BoxDrawingSection

        Evergreen.V3.Types.MarkdownSection ->
            Evergreen.V4.Types.MarkdownSection

        Evergreen.V3.Types.PreviewSection ->
            Evergreen.V4.Types.PreviewSection

        Evergreen.V3.Types.HtmlSection ->
            Evergreen.V4.Types.HtmlSection
