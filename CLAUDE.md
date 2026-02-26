# Tabular - Development Best Practices

## Project Type
- Lamdera full-stack Elm app (table editor with markdown/box-drawing/HTML output)
- Uses `lamdera/program-test` for end-to-end testing

## Git Workflow
- Implement features on feature branches, not directly on main
- Merge to main via pull requests
- After pushing, always run `gh pr checks <number> --watch` and print the CI results

## Key Commands
- **Compile**: `npm run build` (or `lamdera make src/Frontend.elm --output=/dev/null`)
- **Run tests**: `npm test` (or `elm-test --compiler lamdera`)
- **Run E2E**: `npm run test:e2e` (or `npx playwright test`)
- **Review**: `npm run review` (or `elm-review`)
- **All checks**: `npm run check`
- **elm-test init**: Must use `--compiler $(which lamdera)` flag due to lamdera package constraints
- Prefer running tests via npm scripts, not raw commands

## Custom Types Over Primitives
- Use custom types (tagged unions) wherever possible
- Strings, integers, and booleans should only be used for things that cannot be represented otherwise
- For Sets/Dicts keyed by custom types, prefer `lamdera/containers` (`SeqSet`, `SeqDict`) — these are Lamdera-serializable
- Avoid `rtfeldman/elm-sorter-experiment` (`Sort.Set`/`Sort.Dict`) in the model — they store functions internally, which Lamdera cannot serialize

## Accessibility (WCAG)
- Interactive elements must be `<button>` unless they navigate to a URL (then `<a>`)
- Never use `<div>` or `<span>` with `onClick` for interactive behavior

## Make Impossible States Impossible
- Design types so illegal states are unrepresentable
- Prefer custom types and record structures that enforce invariants at compile time

## Effect Module Migration Pattern
- `lamdera/program-test` requires `Effect.*` module wrappers
- `Effect.Lamdera.frontend` takes `Lamdera.sendToBackend` as first arg, then `app_` config record
- `Effect.Lamdera.backend` takes `Lamdera.broadcast`, `Lamdera.sendToFrontend`, then `app_`
- Type sigs: `Command FrontendOnly ToBackend FrontendMsg` (not generic `restriction toMsg`)
- `Browser.Internal`/`Browser.External` patterns stay as `Browser.*`, not `Effect.Browser.*`
- `UrlRequest` is a type alias for `Browser.UrlRequest`, not a custom type with (..) constructors

## Testing Notes
- `elm-explorations/test` v2.x removed `Expect.true`/`Expect.false` — use `Expect.equal True/False (expr)` instead
- Program tests: textarea values can't be found via `Test.Html.Selector.text`; use `Test.Html.Selector.attribute (Html.Attributes.value ...)` on specific elements
- CSS classes like "format-btn active" can be checked with `Test.Html.Selector.class "active"`
