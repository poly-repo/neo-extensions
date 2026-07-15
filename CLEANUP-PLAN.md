# Extensions Cleanup Plan

Audit of every extension under `extensions/extensions/{mav,neo}/` (~109 non-test
`.el` files across 24 extensions — findings below are scoped to the ~22 of those
that are actually reachable from a default-configured NEO instance; see
[Not used](#not-used-excluded-from-findings)), looking for:

1. Raw `(use-package ...)` calls that should be `(neo/use-package ...)`.
2. Configuration of third-party packages done outside any `neo/use-package` form
   (top-level `setq`, `add-hook`, `advice-add`, `define-key`, `global-set-key`,
   `with-eval-after-load`, etc.).
3. For each, whether there's a legitimate reason it sits outside
   `neo/use-package` — `neo/use-package` (`core/neo-use-package.el`) does not
   evaluate its `use-package` form immediately when `neo/use-extensions` is
   non-nil (the normal running state); it pushes the form onto
   `neo--enabled-packages`, replayed later by
   `neo/replay-extension-packages`/`neo/replay-installed-extensions-packages`.
   That's an extra layer of deferral on top of `use-package`'s own
   `:init`/`:config`/`:after`. Code that must run eagerly at extension-load time
   (registering into NEO's own core APIs, defining the extension's own
   functions/vars, `with-eval-after-load` guards for packages the extension
   doesn't own) has a legitimate reason to sit outside it. A bare top-level
   `setq`/`add-hook`/`advice-add` touching a **third-party** package's
   variables, with no such guard, does not — it's a load-order hazard: it runs
   before that package is necessarily installed/loaded, and duplicates or drifts
   from whatever the package's own `neo/use-package` block does.

Settings that only touch the extension's own code (its own defvars, defcustoms,
hooks tied to its own functions, calls into `neo/register-*` core APIs) are not
flagged — that's normal extension logic, not "package configuration outside
neo/use-package."

Method: four parallel sub-audits, one per group below, each independently
reading `core/neo-use-package.el` for grounding, then grepping/reading every
file in its assigned extensions. Two of the highest-stakes findings (the
`treemacs` duplicate/conflict and the `cocktails` tempel bug) were independently
re-verified against the source tree and the vendored `tempel.el` build.

**Usage filter (added in a follow-up pass):** every finding below was then
checked against what actually runs. "Used" is defined as: reachable from the
default `enabled-extensions` config value, `("neo:full-monty")`
(`core/neo-early-init-utils.el:182`), expanded through each manifest's
`:requires` closure, and — within each used extension — actually reached by a
`(require ...)` chain starting from that extension's entry file (an extension
can contain a sibling `.el` file that exists on disk but is never `require`d by
anything, in which case its `neo/use-package`/config never runs). This turned up
two wholly unused extensions and one dead file inside an otherwise-used
extension; see [Not used](#not-used-excluded-from-findings) for the full list
and what changed as a result.

## Not used (excluded from findings)

Three things audited in an earlier pass turned out to be unreachable from a
normal running NEO instance, so they're excluded from the findings below
(mentioned only where needed for context) rather than treated as live bugs:

| What                                                                                                                                 | Why it's unused                                                                                                                                                                                                                                                                                                                     | Disposition                                                                                                                                                                                                                                                                                                   |
| ------------------------------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `extensions/extensions/neo/mlody/` (whole extension: `neo-mlody.el`, `neo-mlody-shell.el`, `neo-mlody-starlark-mode.el`)             | Not in `full-monty`'s `:requires` closure and not required by any other extension's manifest. The only repo-wide reference to the slug `"neo:mlody"` is a synthetic fixture name in `core/neo-treesit-test.el:28-35` (a unit test double, not a real load path) — the extension is superseded by `neo/mlody-mode`, which _is_ used. | Not audited for findings (its `neo-mlody-shell.el:69-71` `with-eval-after-load 'eshell` guard would otherwise have been legitimate, for the record). Recommend deleting the directory once confirmed nothing outside this repo still enables `neo:mlody`, mirroring the `workflow`→`neo-workflow` case below. |
| `extensions/extensions/neo/workflow/` (whole extension, 14 files)                                                                    | Already covered as priority finding 4 — zero references to `neo:workflow` outside its own directory; superseded by `neo/neo-workflow/`.                                                                                                                                                                                             | Findings kept (see [workflow vs neo-workflow](#workflow-vs-neo-workflow)) specifically to justify deletion, not because it's live.                                                                                                                                                                            |
| `extensions/extensions/neo/ui/neo-ui-treemacs.el` (single file, formerly inside the otherwise-used `neo/ui` extension) — **deleted** | `neo/ui`'s entry file, `neo-ui.el`, only `require`d `neo-ui-frame`, `neo-ui-fonts`, `neo-ui-themes`, `neo-ui-modeline`, and `neo-ui-side-windows`. A repo-wide grep for `neo-ui-treemacs` found only the file's own `(provide 'neo-ui-treemacs)` — nothing required it.                                                             | **Resolved.** Its two live-and-worth-keeping settings, `(treemacs-git-mode 'deferred)` and `(treemacs-git-commit-diff-mode 1)`, were ported into `neo/projects/neo-projects.el`'s `treemacs` `:config` block; the file was then deleted. See [ui vs projects](#neoui-vs-neoprojects-treemacs-family).         |

Everything else referenced in this document — every remaining extension in
`extensions/extensions/{mav,neo}/` and every file/line cited below — was
confirmed reachable: either directly in `full-monty`'s `:requires` closure (the
default `enabled-extensions` value is `("neo:full-monty")`,
`core/neo-early-init-utils.el:182`) or transitively via another used extension's
manifest, and every specific file cited is actually `require`d somewhere in its
extension's own `require` chain starting from its entry file.

## Priority findings (fix these)

1. **RESOLVED.** `neo/ui/neo-ui-treemacs.el` was dead code — never `require`d by
   `neo-ui.el`, so its `neo/use-package treemacs` block never ran, and there was
   no live conflict with `neo/projects`'s treemacs config (an earlier pass in
   this plan wrongly called this a live, active conflict — see
   [Not used](#not-used-excluded-from-findings)). Its two features that _were_
   actually live and worth keeping — `(treemacs-git-mode 'deferred)` and
   `(treemacs-git-commit-diff-mode 1)` — were grafted into
   `neo/projects/neo-projects.el`'s `treemacs` `:config` block; the `M-0` →
   `treemacs-select-window` winum binding was deliberately dropped (superseded
   by `S-left`/`S-right` window navigation already in use); the file itself was
   deleted. See [ui vs projects](#neoui-vs-neoprojects-treemacs-family) below.
2. **`mav/cocktails/neo-cocktails.el` configures the wrong tempel variables** —
   `tempel-paths`/`tempel-file` instead of the real
   `tempel-path`/`tempel-path-templates` — because the config lives outside the
   `neo/use-package tempel` block instead of in its `:custom`. Confirmed against
   the vendored `tempel.el` build
   (`/home/mav/.cache/neo-devel/elpaca/builds/tempel/tempel.el:58,567`). Also
   hardcodes a personal, ephemeral worktree path. See
   [mav/cocktails](#mavcocktails).
3. **`neo-programming-foundation.el` defines `neo--eglot-format-if-supported`
   twice** (lines 332-337 and 339-344); the second silently shadows the first,
   and the resulting function is then wired up via _both_ a direct
   `before-save-hook` add (outside any `neo/use-package` block) _and_ the
   correct `:hook (eglot-managed-mode . neo/eglot-format-on-save)` inside the
   `eglot` block — redundant and confusing. See
   [programming-foundation](#programming-foundation).
4. **`extensions/extensions/neo/workflow/` is dead code**, fully superseded by
   `extensions/extensions/neo/neo-workflow/`. Zero references to `neo:workflow`
   exist anywhere outside the directory itself; `full-monty`'s manifest (the
   "complete NEO experience" bundle) requires `neo:neo-workflow`, not
   `neo:workflow`. Recommend deleting the whole directory (14 non-test files +
   its own `tests/`). See [workflow vs neo-workflow](#workflow-vs-neo-workflow).
5. **RESOLVED (omega-11sv.13.5): `neo/use-package`'s `:if`/`:unless`/`:disable`
   keywords didn't survive cross-extension merging.** A single extension's own
   `:if`/`:when`/`:unless`/`:disabled` always worked; the bug was in
   `neo--merge-use-package-declarations` treating them as a naive
   concatenate-and-dedup list section, which produced a malformed
   `:if COND1 COND2` when two extensions declared the same package with
   different conditions. Fixed with a dedicated merge strategy in
   `core/neo-use-package.el`. See
   [neo/use-package gap](#neouse-package-gap-ifunlessdisable).

## Cross-cutting patterns

- **Duplicate `neo/use-package` declarations for the same third-party package
  across sibling extensions with no `:requires` edge between them** is a
  recurring risk class, not a one-off — this is exactly what caused `omega-45pw`
  (`transient` declared independently in `neo-better-git.el` and
  `neo-programming-foundation.el`, already fixed). One instance found here,
  `treemacs` / `treemacs-all-the-icons` / `treemacs-magit` (`neo/ui` vs
  `neo/projects`), is **not currently live** — `neo/ui`'s copy
  (`neo-ui-treemacs.el`) is dead code, never `require`d, so only
  `neo/projects`'s declaration actually runs today (see priority finding 1 and
  [Not used](#not-used-excluded-from-findings)). It would become a live instance
  of this exact hazard the moment someone wires the file back in without also
  resolving the conflicting values. A second, still-latent (not yet live either
  way) instance: `vterm` is declared in
  `neo/ai-buddy/neo-ai-buddy-codex.el:154`, and a second, currently
  commented-out `neo/use-package vterm` sits in
  `neo/terminal/neo-terminal.el:291-294` — harmless today, but a trap if that
  block is ever uncommented without adding a `:requires` edge between `ai-buddy`
  and `terminal`.
- **The `with-eval-after-load 'somepkg` idiom, for a package the extension
  doesn't own, used to attach the extension's own function/hook/keymap entry, is
  the correct and common legitimate pattern** in this codebase — seen (and
  judged legitimate) in `better-git` (magit, magit-worktree), `python`/`haskell`
  (project.el), `beads/` (evil-mode, project.el), `ui` (key-chord).
  `neo/haskell/neo-haskell.el` is the standout reference example: every
  third-party touch outside a `neo/use-package` block is wrapped this way and
  commented with explicit rationale — worth pointing other extension authors at
  it.
- **The opposite anti-pattern** — configuring a package's _own_ variables (not
  just attaching the extension's hooks/keys to it) via a bare top-level
  `setq`/`add-hook`/`advice-add`, or via a second `with-eval-after-load` block
  disconnected from that package's own `neo/use-package` declaration — recurs in
  several extensions as a milder form of drift: `neo-programming-foundation.el`
  (key-chord/eglot/c++-ts-mode chord, with leftover debug `message` calls),
  `neo-programming-foundation-treesit.el` (treesit-fold-indicators config split
  ~70 lines from the package's own block), `neo/leetcode` (a second
  `with-eval-after-load 'leetcode` block monkey-patching the package separately
  from its own `neo/use-package` block), `neo/latex` (a stray
  `with-eval-after-load 'tex` block that redundantly re-sets values already in
  `:custom`, plus genuinely new settings that belong there too), `neo/terminal`
  (`advice-add` on `eshell/cat` at file-load time, before the `eshell` block is
  replayed; and a stray `eshell-prompt-function` `setq` ~120 lines from that
  block). None of these are as severe as the `treemacs`/`tempel` bugs, but
  they're the same rot pattern and should be folded back into the owning
  `neo/use-package` block during cleanup.
- **Vendored, not package-managed, third-party code**:
  `programming-foundation/beads/` is a wholesale copy of an upstream package
  (`beads.el` by Christian Tietze, GPLv3, `codeberg.org/ctietze/beads.el`)
  loaded via a plain `(require 'beads)`, not `neo/use-package`. Its own two real
  dependencies (`transient`, `vui`) are correctly declared via `neo/use-package`
  in the parent file. Not a bug, but a structural outlier worth a deliberate
  decision (document why it's vendored, or move to a real `:ensure` install if
  upstream supports it).
- **Dead/unused functionality found in both `workflow/` and `neo-workflow/`**:
  `neo/workflow-project-enable`/`-disable` (`neo-workflow-project.el`) register
  a `project-find-functions` backend but are never called anywhere in either
  version of the extension — independent of the workflow-vs-neo-workflow
  duplication, this looks like a loose end in the current (`neo-workflow`)
  extension too.
- **Commented-out dead code** is heavy in a few files and worth a pass:
  `neo-programming-foundation.el` (lines 359-364, 436-440, 477-521 — a whole
  alternate `eglot` block with a stray trailing-`d` typo, and a raw
  `use-package highlight-symbol`, inert since commented),
  `neo-programming-foundation-treesit.el:94-99`,
  `better-git/neo-better-git-brancher.el:172-276` (~110 lines of commented-out
  duplicate defuns and stale eval logs with absolute paths/timestamps).

## Per-extension findings

### `programming-foundation`

| Location                                                                        | Kind                             | Touches                               | Verdict    | Note                                                                                                                                                                                                                                                                                                                                                 |
| ------------------------------------------------------------------------------- | -------------------------------- | ------------------------------------- | ---------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `neo-programming-foundation.el:346`                                             | outside-hook                     | eglot (`before-save-hook`)            | suspect    | Redundant with the correct `:hook (eglot-managed-mode . neo/eglot-format-on-save)` already in the `eglot` block (line 374); also calls the duplicated function below.                                                                                                                                                                                |
| `neo-programming-foundation.el:332-344`                                         | dead code                        | —                                     | suspect    | `neo--eglot-format-if-supported` defined twice; second silently shadows first.                                                                                                                                                                                                                                                                       |
| `neo-programming-foundation.el:537`                                             | outside-setq (`global-set-key`)  | flymake (via own wrapper command)     | unclear    | Binds NEO's own command but outside flymake's (empty) `neo/use-package` block; stylistic, not a hazard.                                                                                                                                                                                                                                              |
| `neo-programming-foundation.el:547-552`                                         | outside-eval-after-load          | key-chord, eglot, c++-ts-mode         | suspect    | Triple-nested `with-eval-after-load`, `key-chord` never declared via `neo/use-package` anywhere in the repo; also contains two leftover debug `message` calls ("DEFINING CHORD", "KEYMAP BOUND").                                                                                                                                                    |
| `neo-programming-foundation-treesit.el:20-92`                                   | outside-eval-after-load / advice | treesit-fold, treesit-fold-indicators | suspect    | Package already has a `neo/use-package` block (lines 14-18); this is 70+ lines of further config (keymap patch, fringe-bitmap redefinition, a non-`neo/`-prefixed function override `treesit-fold-indicators-click-fringe`) disconnected from it. No load-order bug (`with-eval-after-load` defers correctly) but a structural/convention violation. |
| `beads/beads-detail.el`, `beads-form.el`, `beads-hierarchy.el`, `beads-list.el` | outside-eval-after-load          | evil-mode                             | legitimate | Explicitly commented "configure evil IF present, don't enable it"; evil is never `neo/use-package`-declared anywhere, correctly treated as optional soft integration.                                                                                                                                                                                |
| `beads/beads-project.el:85-92`                                                  | outside-hook                     | `project.el` (builtin)                | legitimate | Registers own command into `project-switch-commands`, guarded by `boundp`, deferred via `after-init-hook`.                                                                                                                                                                                                                                           |

Other notes: the entire `beads/` directory is vendored upstream code, see
cross-cutting section above. `neo--bazel-clangd-path`
(`neo-programming-foundation.el:442-459`) hardcodes
`project-root "~/Projects/uno"` with a TODO admitting it should be computed from
`project.el` — a silent-wrong-path landmine for anyone else, unrelated to
package config but worth fixing alongside this cleanup. No duplicate package
declarations found within this extension itself.

### `workflow` vs `neo-workflow`

`workflow/` (old, slug `neo:workflow`) is dead code, fully superseded by
`neo-workflow/` (slug `neo:neo-workflow`) — see priority finding 4. Findings
below are given mainly to justify the deletion and to flag two things that
carried over into the _current_ extension unfixed.

| Location                                                                            | Kind            | Touches                                       | Verdict                | Note                                                                                                                                                                                                                                                                                            |
| ----------------------------------------------------------------------------------- | --------------- | --------------------------------------------- | ---------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `workflow/neo-workflow.el:22`                                                       | raw-use-package | ghub                                          | suspect                | `(use-package ghub :ensure t)`, self-acknowledged: comment right above says "TODO use neo/use/package once we load this properly".                                                                                                                                                              |
| `workflow/neo-workflow.el:24`                                                       | other           | elpaca (`elpaca-wait`)                        | suspect                | Extension calling `elpaca-wait` directly is otherwise unheard-of outside `init.el`/`core/neo-elpaca.el`; paired with a commented-out hardcoded personal worktree path a few lines below.                                                                                                        |
| `workflow/neo-workflow-async.el:4`                                                  | undeclared dep  | async                                         | suspect                | `(require 'async)` with no `neo/use-package`/`use-package` anywhere in the repo; moot since dead code — explains why the rewrite dropped `async` entirely.                                                                                                                                      |
| `workflow/neo-workflow-context.el` + `neo-workflow-status.el` (old)                 | undeclared dep  | magit                                         | unclear                | `magit-toplevel` etc. called directly with no `require`/declaration in `workflow/`; works only because `better-git` (a `:requires` of this extension) happens to pull magit in transitively. The rewrite explicitly removed this dependency ("no magit" per `neo-neo-workflow.el`).             |
| `workflow/neo-workflow-issues.el:12` and `neo-workflow/neo-workflow-issues.el:16`   | neo/use-package | tempel                                        | legitimate             | Same correct pattern in both versions.                                                                                                                                                                                                                                                          |
| `workflow/neo-workflow-project.el:56` and `neo-workflow/neo-workflow-project.el:28` | outside-hook    | `project.el`                                  | unclear                | `add-hook 'project-find-functions` inside an autoloaded, interactive `neo/workflow-project-enable` — never called anywhere in either version. Dead functionality carried over into the _current_ extension; independent follow-up worth filing regardless of the workflow/neo-workflow cleanup. |
| both versions' `neo-workflow-status.el`                                             | other           | vtable                                        | legitimate             | `vtable` bundled with Emacs core since 29.1; plain `require` is fine.                                                                                                                                                                                                                           |
| both versions' `neo-workflow-status.el`                                             | other           | vtable (private `vtable--insert-header-line`) | legitimate but fragile | `cl-letf`-scoped override of a double-dash "private" vtable symbol, explicitly flagged `;; HACK` with rationale (no public API to hide the header); could break silently on a vtable upgrade.                                                                                                   |

Recommend: delete `extensions/extensions/neo/workflow/` entirely (it also keeps
a stray `neo-workflow-db-test.el` outside `tests/`, unlike the new extension's
consistent layout). File a follow-up for the dead `project-find-functions`
registration in the _current_ `neo-workflow/`.

### `neo/ui` vs `neo/projects` (treemacs family) — RESOLVED

`neo/ui/neo-ui-treemacs.el` used to `provide` `'neo-ui-treemacs` and declare
`treemacs`/`treemacs-all-the-icons`/`treemacs-magit` via `neo/use-package` — but
**nothing `require`d it**. `neo/ui`'s entry file (`neo-ui.el`) only requires
`neo-ui-frame`, `neo-ui-fonts`, `neo-ui-themes`, `neo-ui-modeline`, and
`neo-ui-side-windows`; a repo-wide grep for `neo-ui-treemacs` turned up only its
own `(provide ...)` line. So `neo/projects/neo-projects.el`'s
`neo/use-package treemacs` block (and its sibling `treemacs-all-the-icons`/
`treemacs-magit` declarations) was already the only one that ever ran — there
was no live duplicate declaration and no live conflict, despite the two blocks
disagreeing on `treemacs-follow-after-init`, `treemacs-display-in-side-window`,
etc.

Before deleting, the dead file was checked for anything worth carrying over. Two
settings were live and genuinely useful and had no equivalent in `neo/projects`:
`(treemacs-git-mode 'deferred)` and `(treemacs-git-commit-diff-mode 1)`
(git-status decorations and a last-commit diff view in the tree). Those were
ported into `neo/projects/neo-projects.el`'s `treemacs` `:config` block. A third
live setting, an `M-0` → `treemacs-select-window` winum binding, was
deliberately dropped — window navigation is already handled via
`S-left`/`S-right`. Everything else in the file was either identical to
`neo/projects`' live config, a design decision `neo/projects` had already
deliberately superseded (e.g. `treemacs-display-in-side-window`), or
dead-even-within-the-dead-file (a debounced auto-resize-to-frame-fraction helper
whose `add-hook` was itself commented out, and several commented-out
`neo/use-package` blocks for alternatives like
`treemacs-persp`/`project-treemacs` that were never enabled).
`neo-ui-treemacs.el` has been deleted.

### `ai-buddy`

| Location                       | Kind         | Touches   | Verdict | Note                                                                                                                                                                                                                                                                          |
| ------------------------------ | ------------ | --------- | ------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `neo-ai-buddy-gemini.el:56-60` | outside-hook | aidermacs | suspect | `add-hook`/`setq`/`setenv` at top level, unguarded, for a package (`aidermacs`) never declared via `neo/use-package` anywhere. The rest of the file has since migrated to `ai-code` (line 62) — looks like orphaned leftover from a prior integration, not deliberate design. |
| `neo-ai-buddy.el:15`           | other        | ai-code   | unclear | `global-set-key` for `ai-code-menu` (declared via `neo/use-package` in a _different_ file) sits at top level instead of that block's `:bind`. Harmless, stylistic.                                                                                                            |

Other: `ai-buddy`'s manifest correctly documents its `:requires` with inline
rationale citing the exact `omega-45pw` race — good precedent that wasn't
applied to the `ui`/`projects` treemacs case above.

### `better-git`

| Location                                    | Kind                    | Touches               | Verdict    | Note                                                                                                                                             |
| ------------------------------------------- | ----------------------- | --------------------- | ---------- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| `neo-better-git.el:93-95, 258-260, 262-264` | outside-eval-after-load | magit, magit-worktree | legitimate | All guarded by `with-eval-after-load`; `magit-worktree` is a separately-lazy-loaded feature so a second guard is arguably necessary, not sloppy. |

Other: manifest has `:requires ()`, yet the file's own comments admit it relies
on `transient` being supplied by `programming-foundation` "always present
alongside better-git under full-monty" — an informal assumption, not a declared
edge. `neo-better-git-brancher.el:172-276` has ~110 lines of commented-out
duplicate defuns and stale eval-output logs (absolute paths, timestamps) — pure
dead-code cleanup, unrelated to package config.

`extension-manager` and `mlody-mode`: no findings — clean. (The separate
`neo/mlody` extension, which has its own unrelated `neo-mlody-shell.el` with a
legitimate `with-eval-after-load 'eshell` guard, is unused — see
[Not used](#not-used-excluded-from-findings) — so it's excluded from findings
here rather than listed as clean.)

### `neo/terminal`

| Location              | Kind           | Touches                  | Verdict | Note                                                                                                                                                                                                                                        |
| --------------------- | -------------- | ------------------------ | ------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `neo-terminal.el:44`  | outside-advice | `eshell/cat` (em-unix)   | suspect | `advice-add` at file-load time, before the `eshell` `neo/use-package` block (line 143) is even replayed — risks advising a not-yet-defined symbol or being clobbered if eshell later redefines it. Should move into that block's `:config`. |
| `neo-terminal.el:267` | outside-setq   | `eshell-prompt-function` | suspect | Stray `setq` ~120 lines after the `eshell` block's own `:custom`; harmless but scattered, same package configured in two places.                                                                                                            |

### `neo/python`

| Location                | Kind                    | Touches                  | Verdict    | Note                                                                                                                                                                       |
| ----------------------- | ----------------------- | ------------------------ | ---------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `neo-python.el:12`      | outside-eval-after-load | `project.el` (builtin)   | legitimate | Correctly deferred, adds `pyproject.toml` as a root marker.                                                                                                                |
| `neo-python.el:200-202` | comment/finding         | `neo/use-package` itself | resolved   | Comment claiming `:if`/`:unless`/`:disable` "don't seem to work" was stale; root cause was a cross-extension merge bug, fixed in omega-11sv.13.5 — see priority finding 5. |

### `neo/leetcode`

| Location                  | Kind                    | Touches  | Verdict        | Note                                                                                                                                                                                                                        |
| ------------------------- | ----------------------- | -------- | -------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `neo-leetcode.el:132-155` | outside-eval-after-load | leetcode | suspect (mild) | Safe (deferred) but a second, disconnected block monkey-patching the same package that already has its own `neo/use-package` block (lines 109-117) earlier in the file; should be consolidated into that block's `:config`. |

### `neo/latex`

| Location               | Kind                    | Touches        | Verdict | Note                                                                                                                                                                                                                                                    |
| ---------------------- | ----------------------- | -------------- | ------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `neo-latex.el:357-363` | outside-eval-after-load | AUCTeX (`tex`) | suspect | Redundantly re-sets `TeX-source-correlate-*`, already set via `:custom` in the same file's `neo/use-package auctex` block (lines 343-345); the genuinely new settings here (`TeX-view-program-list`/`-selection`) belong in that block's `:config` too. |

### `neo/haskell`

No violations — reference example. Every third-party touch outside a
`neo/use-package` block is either inside a `defun` invoked only via a
hook/`:config`, or a `with-eval-after-load` guard for a package the extension
doesn't own (`project`, `treesit-fold`, `eglot`, `haskell-mode`,
`haskell-ts-mode`, `haskell-cabal`, `align`), each with explicit rationale
comments. Worth pointing other extension authors here.

### `mav/cocktails`

| Location                 | Kind                                   | Touches | Verdict                     | Note                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| ------------------------ | -------------------------------------- | ------- | --------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `neo-cocktails.el:13-19` | outside-setq / outside-eval-after-load | tempel  | **suspect — confirmed bug** | `(defvar tempel-paths '())` / `(add-to-list 'tempel-template-sources 'tempel-file)` / `(add-to-list 'tempel-paths ...)`. Verified against the vendored `tempel.el` build: the real customization variable is `tempel-path` (singular, `tempel.el:58`), and the real template-source function is `tempel-path-templates` (`tempel.el:567`) — `tempel-paths` and `tempel-file` are not real tempel symbols. This config is dead/likely-broken, and exists precisely because it sits outside `(neo/use-package tempel)` (line 10, empty body) instead of in its `:custom`. Also hardcodes a personal, ephemeral worktree path (`mav-263-rework-side-window-management`) that won't exist on other checkouts. |

`neo-cocktails.el:10`'s `neo/use-package tempel` itself is fine and matches the
pattern used correctly in both `workflow`/`neo-workflow`.

### Clean extensions (no findings)

`session`, `questionable-defaults`, `news`, `projects` (aside from the treemacs
duplication above, which is credited to `ui`), `full-monty` (confirmed
intentional 4-line stub), `elisp`, `dashboard`, `compsel`, `build`,
`extension-manager`, `mlody-mode`. `elisp` and `dashboard` both contain useful
comments documenting the same "declare a dependency's `neo/use-package` _before_
a package whose own metadata lists it as a dependency" Elpaca-race gotcha as
`better-git`'s `transient` note — good awareness, no action needed.

No raw `(use-package ...)` calls exist anywhere in this batch of 15 extensions
(only two harmless commented-out ones, in `session` and `python`).

## `neo/use-package` gap: `:if`/`:unless`/`:disable` (RESOLVED, omega-11sv.13.5)

`neo/python/neo-python.el:200-202` carried an explicit code comment that these
`use-package` keywords "don't seem to work," speculating it was related to
`neo/use-package` expanding the underlying `use-package` form at a potentially
unexpected (deferred-replay) time. That theory was wrong: a single extension's
own `:if`/`:when`/`:unless`/`:disabled` already worked correctly. The real gap
was in `neo--merge-use-package-declarations` (`core/neo-use-package.el`), which
treated these four keywords as an ordinary concatenate-and-dedup list section
(like `:hook`/`:config`). When two extensions declared the same package with
different conditions, this produced a malformed
`(use-package NAME :if COND1 COND2 ...)` — `use-package`'s `:if` takes exactly
one form — which threw a parse error at replay time and silently dropped that
package's config for every contributing extension. Fixed with a dedicated merge
strategy (`neo--merge-use-package-condition-section`) that unaliases
`:when`/`:unless` into `:if` per source, AND-combines distinct real conditions
(matching native `use-package`'s own repeated-`:if` semantics), and records a
`neo-merge-conflict` when a merge silently overrides an unconditional source's
expectation. See `core/neo-use-package-test.el`'s
`neo/merge-use-package-declarations-if-*`/`-disabled-*` tests for coverage.

## Suggested follow-ups

Roughly in priority order:

1. ~~Decide `neo-ui-treemacs.el`'s fate~~ — **done**: `treemacs-git-mode`/
   `treemacs-git-commit-diff-mode` ported into `neo/projects/neo-projects.el`,
   dead file deleted.
2. Fix `mav/cocktails/neo-cocktails.el`'s tempel config (correct variable names,
   move into `:custom`, drop the hardcoded personal path).
3. Delete `extensions/extensions/neo/workflow/` (dead code superseded by
   `neo-workflow/`).
4. Fix the duplicate `neo--eglot-format-if-supported` definition and fold the
   stray `before-save-hook` into the `eglot` block in `programming-foundation`.
5. ~~Investigate why `neo/use-package`'s `:if`/`:unless`/`:disable` don't work~~
   — **done**: cross-extension merge bug, fixed in omega-11sv.13.5.
6. Confirm `extensions/extensions/neo/mlody/` (superseded by `mlody-mode`) has
   no remaining real-world enablers, then delete it — same disposition as
   `workflow/`, just not yet promoted to a priority finding since nothing
   currently points at it even accidentally.
7. Lower priority, same-session cleanup candidates: fold the scattered
   `treesit-fold`/`leetcode`/AUCTeX/`eshell` config back into their owning
   `neo/use-package` blocks; delete the dead
   `project-find-functions`/`neo/workflow-project-enable` registration in
   `neo-workflow`; delete the ~110 lines of commented-out dead code in
   `neo-better-git-brancher.el` and the commented-out alternate `eglot` block in
   `neo-programming-foundation.el`; fix the hardcoded `~/Projects/uno` path in
   `neo--bazel-clangd-path`; decide whether `programming-foundation/beads/`
   should stay vendored or move to a real `:ensure` install.
