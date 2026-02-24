# Tabular - Development Best Practices

## Git Workflow
- Implement features on feature branches, not directly on main
- Merge to main via pull requests using `gh pr create`

## Custom Types Over Primitives
- Use custom types (tagged unions) wherever possible
- Strings, integers, and booleans should only be used for things that cannot be represented otherwise
- For Sets/Dicts keyed by custom types, prefer `rtfeldman/elm-sorter-experiment` (`Sort.Set`, `Sort.Dict`)
- Caveat: `Sort.Set`/`Sort.Dict` store functions internally, so Lamdera cannot serialize them in the model. Use `List` for small collections of custom types in the model instead.

## Accessibility (WCAG)
- Interactive elements must be `<button>` unless they navigate to a URL (then `<a>`)
- Never use `<div>` or `<span>` with `onClick` for interactive behavior

## Make Impossible States Impossible
- Design types so illegal states are unrepresentable
- Prefer custom types and record structures that enforce invariants at compile time
