---
description: Start or continue work on the bead bound to the current worktree.
argument-hint: extra instructions
---

Use the current worktree's bead binding as the default work item for this session.

1. Try to read `.codex/bead.json` from the current project root.
2. If the file exists:
   - Extract the bead `id` and inspect it with `bd show <id>`.
   - If the bead is already closed, delete `.codex/bead.json`, tell the user the stale binding was cleared, and continue with step 3.
   - If the bead is already `in_progress`, treat it as the active task immediately and continue with step 4.
   - If the bead is open but not yet in progress, claim it with `bd update <id> --claim`, refresh `.codex/bead.json` if needed, and then continue with step 4.
3. If there is no active binding after step 2, ask the user which bead they want to work on. Do not guess.
   - After the user supplies a bead id, inspect it with `bd show <id>`.
   - If that bead is already closed, tell the user it cannot be resumed and ask for another bead.
   - If the bead is not yet in progress, claim it before writing the binding.
   - Then bind the current worktree by writing `.codex/bead.json` with this schema:
     `{"id":"<id>","title":"<title>","status":"<status>","repo_root":"<repo_root>","worktree_root":"<worktree_root>","selected_at":"<local timestamp with offset>"}`
   - Use the current repository root for `repo_root` and the current project/worktree root for `worktree_root`.
   - Use the post-claim bead status when filling the `status` field.
4. Continue or start the work described by the active bead, using any extra user instructions below.
5. If the bead becomes closed during this session, delete `.codex/bead.json` before finishing so later `/work` invocations do not resume a completed bead.

Extra user instructions:
$ARGUMENTS
