Merge the current branch's pull request into main.

Steps:
1. Find the PR number for the current branch using `gh pr view --json number -q .number`.
2. Check that all CI checks have passed with `gh pr checks`.
3. If checks are still running, wait with `gh pr checks <number> --watch` and print results.
4. If all checks pass, squash-merge with `gh pr merge <number> --squash`.
5. Switch back to main and pull: `git checkout main && git pull origin main`.
6. Monitor the CI run on the merge commit using `gh run list --branch main --limit 1` to find the run, then `gh run watch <run-id>` and print the results.
7. Print confirmation with the merged PR number and URL.

If $ARGUMENTS is provided, treat it as the PR number instead of looking it up from the current branch.
