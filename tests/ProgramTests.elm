module ProgramTests exposing (suite)

import Backend
import Effect.Browser.Dom as Dom
import Effect.Lamdera
import Effect.Test exposing (FileUpload(..), HttpResponse(..), MultipleFilesUpload(..))
import Effect.Time
import Frontend
import Html.Attributes
import Test exposing (Test)
import Test.Html.Query
import Test.Html.Selector
import Types exposing (BackendModel, BackendMsg, FrontendModel, FrontendMsg, ToBackend, ToFrontend)
import Url exposing (Url)


unsafeUrl : Url
unsafeUrl =
    case Url.fromString "https://tabular.lamdera.app" of
        Just url ->
            url

        Nothing ->
            Debug.todo "Invalid url"


config : Effect.Test.Config ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel
config =
    { frontendApp = Frontend.app_
    , backendApp = Backend.app_
    , handleHttpRequest = always NetworkErrorResponse
    , handlePortToJs = always Nothing
    , handleFileUpload = always UnhandledFileUpload
    , handleMultipleFilesUpload = always UnhandledMultiFileUpload
    , domain = unsafeUrl
    }


tests : List (Effect.Test.EndToEndTest ToBackend FrontendMsg FrontendModel ToFrontend BackendMsg BackendModel)
tests =
    [ -- 1. Initial render: 3x3 grid with default headers
      Effect.Test.start
        "Initial render shows 3x3 grid with default headers"
        (Effect.Time.millisToPosix 0)
        config
        [ Effect.Test.connectFrontend
            100
            (Effect.Lamdera.sessionIdFromString "s0")
            "/"
            { width = 800, height = 600 }
            (\client ->
                [ client.checkView 100
                    (Test.Html.Query.has
                        [ Test.Html.Selector.id "cell-0-0"
                        , Test.Html.Selector.id "cell-0-1"
                        , Test.Html.Selector.id "cell-0-2"
                        , Test.Html.Selector.id "cell-2-2"
                        ]
                    )
                ]
            )
        ]

    -- 2. Edit a cell: input into a cell and verify value is set
    , Effect.Test.start
        "Edit a cell updates its value"
        (Effect.Time.millisToPosix 0)
        config
        [ Effect.Test.connectFrontend
            100
            (Effect.Lamdera.sessionIdFromString "s0")
            "/"
            { width = 800, height = 600 }
            (\client ->
                [ client.input 100 (Dom.id "cell-1-0") "hello"
                , client.checkView 100
                    (Test.Html.Query.find [ Test.Html.Selector.id "cell-1-0" ]
                        >> Test.Html.Query.has [ Test.Html.Selector.attribute (Html.Attributes.value "hello") ]
                    )
                ]
            )
        ]

    -- 3. Add row: click + Row, verify 4th row appears
    , Effect.Test.start
        "Add row creates a new row"
        (Effect.Time.millisToPosix 0)
        config
        [ Effect.Test.connectFrontend
            100
            (Effect.Lamdera.sessionIdFromString "s0")
            "/"
            { width = 800, height = 600 }
            (\client ->
                [ client.click 100 (Dom.id "add-row")
                , client.checkView 100
                    (Test.Html.Query.has
                        [ Test.Html.Selector.id "cell-3-0" ]
                    )
                ]
            )
        ]

    -- 4. Add column: click + Column, verify 4th column appears
    , Effect.Test.start
        "Add column creates a new column"
        (Effect.Time.millisToPosix 0)
        config
        [ Effect.Test.connectFrontend
            100
            (Effect.Lamdera.sessionIdFromString "s0")
            "/"
            { width = 800, height = 600 }
            (\client ->
                [ client.click 100 (Dom.id "add-column")
                , client.checkView 100
                    (Test.Html.Query.has
                        [ Test.Html.Selector.id "cell-0-3" ]
                    )
                ]
            )
        ]

    -- 5. Remove row: click delete, verify row removed
    , Effect.Test.start
        "Remove row removes the row"
        (Effect.Time.millisToPosix 0)
        config
        [ Effect.Test.connectFrontend
            100
            (Effect.Lamdera.sessionIdFromString "s0")
            "/"
            { width = 800, height = 600 }
            (\client ->
                [ client.click 100 (Dom.id "del-row-2")
                , client.checkView 100
                    (Test.Html.Query.hasNot
                        [ Test.Html.Selector.id "cell-2-0" ]
                    )
                ]
            )
        ]

    -- 6. Remove column: click delete, verify column removed
    , Effect.Test.start
        "Remove column removes the column"
        (Effect.Time.millisToPosix 0)
        config
        [ Effect.Test.connectFrontend
            100
            (Effect.Lamdera.sessionIdFromString "s0")
            "/"
            { width = 800, height = 600 }
            (\client ->
                [ client.click 100 (Dom.id "del-col-2")
                , client.checkView 100
                    (Test.Html.Query.hasNot
                        [ Test.Html.Selector.id "cell-0-2" ]
                    )
                ]
            )
        ]

    -- 7. Switch format: click Compact, verify button becomes active
    , Effect.Test.start
        "Switch to Compact format activates the button"
        (Effect.Time.millisToPosix 0)
        config
        [ Effect.Test.connectFrontend
            100
            (Effect.Lamdera.sessionIdFromString "s0")
            "/"
            { width = 800, height = 600 }
            (\client ->
                [ client.click 100 (Dom.id "format-compact")
                , client.checkView 100
                    (Test.Html.Query.find [ Test.Html.Selector.id "format-compact" ]
                        >> Test.Html.Query.has [ Test.Html.Selector.class "active" ]
                    )
                ]
            )
        ]

    -- 8. Set alignment: click body center button, verify it becomes active
    , Effect.Test.start
        "Set body center alignment activates the button"
        (Effect.Time.millisToPosix 0)
        config
        [ Effect.Test.connectFrontend
            100
            (Effect.Lamdera.sessionIdFromString "s0")
            "/"
            { width = 800, height = 600 }
            (\client ->
                [ client.click 100 (Dom.id "balign-0-c")
                , client.checkView 100
                    (Test.Html.Query.find [ Test.Html.Selector.id "balign-0-c" ]
                        >> Test.Html.Query.has [ Test.Html.Selector.class "active" ]
                    )
                ]
            )
        ]

    -- 9. Import CSV: open import, enter data, import, verify cell values
    , Effect.Test.start
        "Import CSV populates the grid"
        (Effect.Time.millisToPosix 0)
        config
        [ Effect.Test.connectFrontend
            100
            (Effect.Lamdera.sessionIdFromString "s0")
            "/"
            { width = 800, height = 600 }
            (\client ->
                [ client.click 100 (Dom.id "toggle-import")
                , client.input 100 (Dom.id "import-textarea") "Name,Age\nAlice,30\nBob,25"
                , client.click 100 (Dom.id "import-btn")
                , client.checkView 100
                    (Test.Html.Query.find [ Test.Html.Selector.id "cell-0-0" ]
                        >> Test.Html.Query.has [ Test.Html.Selector.attribute (Html.Attributes.value "Name") ]
                    )
                , client.checkView 100
                    (Test.Html.Query.find [ Test.Html.Selector.id "cell-1-0" ]
                        >> Test.Html.Query.has [ Test.Html.Selector.attribute (Html.Attributes.value "Alice") ]
                    )
                , client.checkView 100
                    (Test.Html.Query.find [ Test.Html.Selector.id "cell-2-1" ]
                        >> Test.Html.Query.has [ Test.Html.Selector.attribute (Html.Attributes.value "25") ]
                    )
                ]
            )
        ]
    ]


suite : Test
suite =
    tests
        |> List.map Effect.Test.toTest
        |> Test.describe "Program Tests"
