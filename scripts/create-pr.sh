#!/usr/bin/env bash
set -euo pipefail

# Create a GitHub PR with an auto-generated screenshot of the app.
# Usage: scripts/create-pr.sh --title "PR title" --body "PR body text"

TITLE=""
BODY=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --title) TITLE="$2"; shift 2 ;;
    --body)  BODY="$2";  shift 2 ;;
    *) echo "Unknown option: $1" >&2; exit 1 ;;
  esac
done

if [[ -z "$TITLE" ]]; then
  echo "Error: --title is required" >&2
  exit 1
fi

SCREENSHOT=$(mktemp /tmp/pr-screenshot-XXXXXX.png)
LAMDERA_PID=""

cleanup() {
  [[ -n "$LAMDERA_PID" ]] && kill "$LAMDERA_PID" 2>/dev/null || true
  rm -f "$SCREENSHOT"
}
trap cleanup EXIT

# Start lamdera live in background
echo "Starting lamdera live..."
lamdera live > /dev/null 2>&1 &
LAMDERA_PID=$!

# Wait for the server to be ready
echo "Waiting for server on port 8000..."
for i in $(seq 1 30); do
  if curl -sf http://localhost:8000 > /dev/null 2>&1; then
    break
  fi
  if [[ $i -eq 30 ]]; then
    echo "Error: lamdera live did not start within 30 seconds" >&2
    exit 1
  fi
  sleep 1
done

# Take screenshot using Playwright
echo "Taking screenshot..."
node -e "
const { chromium } = require('playwright');
(async () => {
  const browser = await chromium.launch();
  const page = await browser.newPage({ viewport: { width: 960, height: 800 } });
  await page.goto('http://localhost:8000');
  // Hide Lamdera dev overlay
  await page.evaluate(() => {
    document.querySelectorAll('[style*=\"position: fixed\"]').forEach(el => el.remove());
  });
  await page.screenshot({ path: process.argv[1], fullPage: true });
  await browser.close();
})();
" "$SCREENSHOT"

# Kill lamdera now that we have the screenshot
kill "$LAMDERA_PID" 2>/dev/null || true
LAMDERA_PID=""

# Upload screenshot to a pr-assets draft release
RELEASE_TAG="pr-assets"
REPO=$(gh repo view --json nameWithOwner -q .nameWithOwner)

if ! gh release view "$RELEASE_TAG" > /dev/null 2>&1; then
  echo "Creating draft release '$RELEASE_TAG'..."
  gh release create "$RELEASE_TAG" --draft --title "PR Assets" --notes "Auto-generated screenshots for pull requests"
fi

TIMESTAMP=$(date +%Y%m%d-%H%M%S)
BRANCH=$(git branch --show-current)
ASSET_NAME="screenshot-${BRANCH}-${TIMESTAMP}.png"

echo "Uploading screenshot..."
gh release upload "$RELEASE_TAG" "${SCREENSHOT}#${ASSET_NAME}" --clobber

SCREENSHOT_URL="https://github.com/${REPO}/releases/download/${RELEASE_TAG}/${ASSET_NAME}"

# Compose the full PR body
PR_BODY="${BODY}

## Screenshot

![Screenshot](${SCREENSHOT_URL})"

# Create the PR
echo "Creating pull request..."
gh pr create --title "$TITLE" --body "$PR_BODY"
