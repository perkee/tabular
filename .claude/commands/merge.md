Merge the current branch's pull request into main.

Run `.claude/scripts/merge.sh $ARGUMENTS`.

If the script exits with code 2 (migration needed), it will have checked out the migration branch and printed file paths. Then:

1. Read the three files printed by the script (MIGRATION_FILE, NEW_TYPES, OLD_TYPES).
2. Fill in any `Unimplemented` placeholders in the migration based on the type differences.
3. Run `npm run build` to verify the migration compiles.
4. Run `.claude/scripts/merge-migration.sh` to commit, push, mark the PR ready, and watch CI.
