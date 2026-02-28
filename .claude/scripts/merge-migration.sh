#!/usr/bin/env bash
set -euo pipefail

BRANCH=$(git branch --show-current)
VERSION=$(echo "$BRANCH" | grep -oE '[0-9]+$')

echo "==> Committing V${VERSION} migration..."
git add "src/Evergreen/Migrate/V${VERSION}.elm"
git commit -m "Fill in V${VERSION} Evergreen migration

Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>"

echo "==> Pushing..."
git push origin "$BRANCH"

echo "==> Marking PR as ready..."
PR_NUMBER=$(gh pr view --json number -q .number)
gh pr ready "$PR_NUMBER"

echo "==> Monitoring CI on migration PR #$PR_NUMBER..."
gh pr checks "$PR_NUMBER" --watch

echo "==> Migration PR #$PR_NUMBER passed CI."
