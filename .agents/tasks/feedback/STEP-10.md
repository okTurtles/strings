# Form a list of the good comments and resolve them

Status: COMPLETED

## Valid Comments Resolved

| # | File | Issue | Resolution |
|---|------|-------|------------|
| 1 | `src/quickjs/dune:59` | `PAWTHS` typo | Renamed to `PATHS` |
| 2 | `.github/workflows/test.yml:33` | `curl` without fail-fast flags | Changed to `curl -fsSL ... -o quickjs.tar.xz` |
| 3 | `tests/dune:41-53` | Side-effecting block clobbers `strings/french.strings` in repo root | Removed the entire "populate root strings/" block |
| 4 | `DEVELOPMENT.md:72-81` | Claims `dune runtest` populates `strings/` dir | Rewritten to accurately describe hermetic test behavior |
| 5 | `ARCHITECTURE.md:73-76` | Says `ppx_expect`; conflates SZXX+Angstrom for Pug | Fixed to `ppx_inline_test` + `ppx_assert`; separated SZXX (HTML) from Angstrom (Pug) |

## Sub tasks

1. [x] Implement fix 1: Rename `PAWTHS` → `PATHS` in `src/quickjs/dune`
2. [x] Implement fix 2: Use `curl -fsSL` in `.github/workflows/test.yml`
3. [x] Implement fix 3: Remove side-effecting block from `tests/dune`
4. [x] Implement fix 4: Update `DEVELOPMENT.md` testing section
5. [x] Implement fix 5: Fix `ARCHITECTURE.md` test tooling description
6. [x] Run tests — all pass

## NOTES

All 5 Copilot comments were valid and resolved. Tests pass after changes. The most substantive fix was removing the side-effecting block from `tests/dune` that would clobber `strings/french.strings` on every test run.
