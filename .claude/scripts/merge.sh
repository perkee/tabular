#!/usr/bin/env bash
set -euo pipefail

PR_NUMBER="${1:-}"

# Step 1: Resolve PR number
if [ -z "$PR_NUMBER" ]; then
  PR_NUMBER=$(gh pr view --json number -q .number)
fi
echo "==> PR #$PR_NUMBER"

# Step 2: Wait for CI checks to pass
echo "==> Waiting for CI checks..."
if ! gh pr checks "$PR_NUMBER" --watch; then
  echo "ERROR: CI checks failed on PR #$PR_NUMBER" >&2
  exit 1
fi

# Step 3: Squash-merge
echo "==> Squash-merging PR #$PR_NUMBER..."
gh pr merge "$PR_NUMBER" --squash

# Step 4: Checkout main and pull
echo "==> Switching to main..."
git checkout main && git pull origin main

# Step 5: Find and watch the CI run on the merge commit
MERGE_SHA=$(git rev-parse HEAD)
echo "==> Waiting for CI run on commit ${MERGE_SHA:0:7}..."
while true; do
  RUN_ID=$(gh run list --branch main --limit 5 --json databaseId,headSha \
    -q ".[] | select(.headSha == \"$MERGE_SHA\") | .databaseId" | head -1)
  if [ -n "$RUN_ID" ]; then
    break
  fi
  sleep 3
done
echo "==> Monitoring CI run $RUN_ID..."
gh run watch "$RUN_ID" || true

# Step 6: Parse job results
RESULTS=$(gh run view "$RUN_ID" --json jobs -q '.jobs[] | "\(.name)=\(.conclusion)"')

CHECK_RESULT=""
DEPLOY_RESULT=""
CLEANUP_RESULT=""
while IFS='=' read -r name conclusion; do
  case "$name" in
    check) CHECK_RESULT="$conclusion" ;;
    deploy) DEPLOY_RESULT="$conclusion" ;;
    cleanup-preview) CLEANUP_RESULT="$conclusion" ;;
  esac
done <<< "$RESULTS"

# Step 7: Report results and handle migration if needed
if [ "$DEPLOY_RESULT" = "success" ]; then
  echo "DEPLOY: Production deploy completed successfully."
fi
if [ "$CLEANUP_RESULT" = "success" ]; then
  echo "CLEANUP: Preview deployment cleaned up."
fi

if [ "$CHECK_RESULT" = "failure" ]; then
  echo "CHECK: Failed — looking for migration branch..."
  git fetch origin
  MIGRATION_BRANCH=$(git branch -r --list 'origin/evergreen/migrate-v*' | sort -V | tail -1 | xargs)

  if [ -n "$MIGRATION_BRANCH" ]; then
    LOCAL_BRANCH="${MIGRATION_BRANCH#origin/}"
    VERSION=$(echo "$LOCAL_BRANCH" | grep -oE '[0-9]+$')
    PREV_VERSION=$((VERSION - 1))

    echo "==> Migration needed: V${PREV_VERSION} -> V${VERSION}"
    if git show-ref --verify --quiet "refs/heads/$LOCAL_BRANCH"; then
      git branch -D "$LOCAL_BRANCH"
    fi
    git checkout -b "$LOCAL_BRANCH" --track "$MIGRATION_BRANCH"

    echo "MIGRATION_FILE=src/Evergreen/Migrate/V${VERSION}.elm"
    echo "NEW_TYPES=src/Evergreen/V${VERSION}/Types.elm"
    echo "OLD_TYPES=src/Evergreen/V${PREV_VERSION}/Types.elm"
    exit 2
  else
    echo "ERROR: check job failed but no migration branch found" >&2
    exit 1
  fi
fi

echo "==> PR #$PR_NUMBER merged and deployed successfully."
