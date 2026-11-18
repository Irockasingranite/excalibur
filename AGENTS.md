# PROJECT KNOWLEDGE BASE

**Generated:** 2026-11-18
**Commit:** jj change xslvqkwr (working copy)
**Branch:** on top of 2fe47090 "flake: Update inputs", ahead of `main`/`devel` (c08ac94b)

## OVERVIEW
Excalibur: Haskell CLI that runs QA checks from `excalibur.yaml` on a git commit range, inside a temporary copy of the repo, and writes a JSON report. Haskell2010 + cabal, built via Nix (haskell-flake); requirements traced with StrictDoc.

## STRUCTURE
```
excalibur/
├── app/Main.hs          # CLI (optparse-applicative), config load, exit code
├── src/
│   ├── Checks.hs        # runChecks: orchestration (repo checks, then per-commit)
│   ├── Checks/          # runGlobalCheck / runFileCheck (ReaderT CheckContext IO)
│   ├── Types.hs         # re-exports all Types.* - import this, not submodules
│   ├── Types/           # config/check/report types + JSON instances
│   ├── Util.hs          # forMDList, mkReportSummary, selective re-exports
│   └── Util/            # one module per git/shell/fs operation
├── tests/               # unit suite `excalibur-test` (tasty + HUnit + QuickCheck)
├── integration-tests/   # `excalibur-integration-tests`, needs real `git` (GitFixture)
├── doc/requirements.sdoc # REQ-*/SPEC-* source of truth (StrictDoc, grammar.sgra)
└── excalibur.yaml       # example config used for testing, NOT this repo's checks
```

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| New CLI option | `app/Main.hs` `parseOptions` | add SPEC + `@relation` range markers |
| Config schema change | `src/Types/Check*.hs` | `FromJSON` + `ToJSON` both; YAML keys are kebab-case |
| New check kind | `Types/Check.hs` `Check` sum + `Checks.hs` `runCheck` | `FromJSON Check` tries variants in order |
| Report JSON shape | `src/Types/CheckReport.hs` | keys `on-repository`/`on-commit`, `expected-exit`/`actual-exit` |
| Command variables | `src/Util/ExpandVariables.hs` | `${filename}`, `${commit-range}` only |
| Git interaction | `src/Util/{ResolveCommitRange,GetChangedFiles,CheckoutCommit}.hs` | shells out via `Util.RunCommand` |
| Temp repo copy | `src/Util/TempCopy.hs` | SPEC-4 / REQ-3 non-interference |
| Requirements | `doc/requirements.sdoc` | |

## CODE MAP
| Symbol | Type | Location | Refs (files) | Role |
|--------|------|----------|------|------|
| `runChecks` | fn | `src/Checks.hs` | 2 | top-level run, wraps `inTempCopy` |
| `CheckContext` | record | `src/Types/Check.hs` | 4 | Reader env: dir, commit, range, vars |
| `CheckReport` / `Report` | records | `src/Types/CheckReport.hs` | 6 | results, DList-backed |
| `CheckVariables` | record | `src/Types/Check.hs` | 5 | substitution values |
| `runCommandIn` | fn | `src/Util/RunCommand.hs` | 6 | `shell` + cwd; stderr DISCARDED |
| `runCommandWithStderrIn` | fn | `src/Util/RunCommand.hs` | 3 | interleaved stdout/stderr; used for check commands |
| `getFinal` | fn | `src/Util/ResolveCommitRange.hs` | 5 | last commit, `"HEAD"` on empty |
| `parseExitCode` / `formatExitCode` | fn | `src/Types/Base.hs` | 4 / 5 | 0 <-> `ExitSuccess`; exit codes serialized as strings |

## CONVENTIONS
- Records use `NoFieldSelectors` + `OverloadedRecordDot` (`x.field`); optics labels via `makeFieldLabelsNoPrefix` (`#field`). `DuplicateRecordFields` is common (e.g. `name`, `command`).
- Explicit export lists everywhere (`-Wmissing-export-lists`); exceptions: `Types.Check`, `Types.CheckReport`.
- Every new module MUST be listed in `excalibur.cabal` (`exposed-modules` / test `other-modules`), else `-Wmissing-home-modules`.
- Accumulate with `DList` (`forMDList`), not list append.
- Style (no formatter config committed): 4-space indent, leading commas, `qualified` imports for Text/ByteString, `-- |` haddock-ish comments.
- Traceability: code implementing a spec carries `-- Implements SPEC-N @relation(SPEC-N, scope=file)` or a `scope=range_start` / `range_end` pair. StrictDoc `source_root_path = "src"`, so markers in `app/` are not picked up by its source traceability.
- SPEC UIDs are not sequential (SPEC-5 absent); pick the next unused number (currently SPEC-15).
- `CHANGELOG.md`: Keep a Changelog, add entries under `## Unreleased`; versioning is Haskell PVP.

## ANTI-PATTERNS (THIS PROJECT)
- Never run checks or `git checkout` in the user's repository - only in the `inTempCopy` dir (0.1.2 fixed exactly this bug).
- Don't ignore `expectedExit` when deciding Success/Failure (fixed bug in 0.1.2 for file checks).
- Don't add a required key to `FileCheck`/`GlobalCheck` without checking the `FromJSON Check` fallback: a parse failure of `FileCheck` silently falls through to `GlobalCheck`.
- Don't treat `excalibur.yaml` here as this repo's QA config.

## UNIQUE STYLES
- Commit list is oldest-first (`git rev-list` output reversed); "final" commit = last element.
- Repo-level checks run once on the final commit; commit checks run per commit with `commitRange = [c]`.
- `${filename}` is `"."` for repo checks; set per file via `withFilename` in file checks.

## COMMANDS
```bash
nix develop                      # dev shell (GHC, cabal, HLS)
cabal build
cabal test all                   # what CI runs: nix develop .#default --command cabal test all
cabal test excalibur-test        # unit tests only
cabal test excalibur-integration-tests   # needs git on PATH
cabal run excalibur -- -r HEAD~3..HEAD -c excalibur.yaml -o report.json .
nix build .#static-excalibur     # static release binary (release.yml, on v* tags)
strictdoc export .               # render requirements (shell.nix provides strictdoc; unverified cmd)
```

## NOTES
- VCS here is Jujutsu (`.jj/`, no colocated `.git`); use `jj --no-pager` non-interactively. The tool itself only understands git repos.
- Nix builds set flag `no-integration-tests` (sandbox has no git repo); integration tests only run via `cabal test` in the dev shell.
- `inTempCopy` creates the temp dir in the CWD (not `/tmp`) and copies `.git` too; it skips its own dir when CWD == repo.
- `getChangedFiles` for a multi-commit range diffs first..last commit, so the first commit's own changes are excluded - check before relying on `changed-only` semantics.
- Config path is resolved relative to the REPOSITORY argument, not the CWD; report output path is relative to CWD.
- `stack.yaml` (lts-23.27) exists but CI/Nix use cabal.
