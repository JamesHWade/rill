## This package

Rill is a personal, source-first feed reader. Preserve source provenance and
keep captured source documents distinct from agent-generated interpretation.
Read `CONTEXT.md` before changing domain language or product boundaries.

## Agent skills

### Issue tracker

Issues, specifications, and the Wayfinder backlog are tracked in GitHub Issues.
See `docs/agents/issue-tracker.md`.

### Triage labels

Use the five canonical triage labels without repository-specific aliases. See
`docs/agents/triage-labels.md`.

### Domain docs

Rill uses a single-context domain layout rooted at `CONTEXT.md`, with
architectural decisions in `docs/adr/`. See `docs/agents/domain.md`.

## Package development

### Key commands

(All these functions have been optimized for agentic use, so they can be called
directly without other arguments.)

```R
# Executing code
devtools::load_all()
code

# Tests
devtools::test() # all tests
devtools::test(filter = "^{name}") # tests for files starting with {name}
devtools::test_active_file("R/{name}.R") # tests for R/{name}.R
devtools::test_active_file("R/{name}.R", desc = "blah") # single exact test

# Test coverage
devtools::test_coverage() # all files
devtools::test_coverage_active_file("R/{name}.R")

# Documentation
devtools::document() # redocument package
pkgdown::check_pkgdown() # check website

# Run complete R CMD check
devtools::check()
```

### Running R

There are three possible ways to run code, listed in rough order of
desirability:

- If you're running inside Posit Assistant or otherwise have an
  `executeCode()` tool available, use it to run code in a session that the user
  can also interact with.
- Otherwise, if an R REPL (for example, `mcp__r__repl` or `btw::run_r`) is
  available, use that. Note that `mcp__r__repl` uses a sandbox that blocks
  network requests and reads or writes outside of the current directory.
- Otherwise, use `Rscript -e "code"`.

### Code style

- Follow the tidyverse style guide.
- Always run `air format .` after generating code.
- Use the base pipe operator (`|>`), not the magrittr pipe (`%>%`).
- Use `\() ...` for single-line anonymous functions. For all other cases, use
  `function() {...}`.

### Test style

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`.
- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing tests.
- Strive to keep tests minimal with few comments.
- Never put code in a `test-{name}.R` file outside a `test_that()` block. Use
  `tests/testthat/helper.R` or `tests/testthat/helper-{name}.R` instead.
- Avoid `expect_true()` and `expect_false()` in favor of specific expectations
  with better failure messages. Useful newer expectations include
  `expect_all_true()`, `expect_all_equal()`, and `expect_r6_class()`.
- Only use `expect_error()` or `expect_warning()` when the condition has a
  known class. Otherwise prefer snapshots so the user can review full output.
- Avoid the `.package` argument to `local_mocked_bindings()`; create a mockable
  wrapper in this package instead of modifying another package's namespace.

### Documentation

- Every user-facing function should be exported and have roxygen2
  documentation.
- Internal functions should not have roxygen documentation.
- Wrap roxygen2 comments to 80 characters.
- Whenever you add a new public documentation topic, add it to `_pkgdown.yml`.
- Always re-document the package after changing a roxygen2 comment.
- Use `pkgdown::check_pkgdown()` to ensure every topic is in the reference
  index.

### `NEWS.md`

- Give every user-facing change a bullet in `NEWS.md`.
- Small documentation changes, internal refactorings, and fixes to bugs
  introduced in the current development version do not need bullets.
- Briefly describe the change to the end user and mention the related issue.
- Keep each bullet on one physical line.
- Put a related function name early in the bullet when applicable.
- Include a GitHub username only for contributors who are not package authors.
- Order function-related bullets alphabetically by function name, with other
  bullets first.

## Specialized skills

- For function or argument deprecation, read the output of
  `usethis::learn_tidy_skill("deprecate")`.
- When adding input checking or a new exported function, read the output of
  `usethis::learn_tidy_skill("arg-checking")`.

## Git

- If the user asks you to commit, use Markdown in the commit message and do not
  line wrap.
- If a commit fixes an issue, include `Fixes #num.` on its own line.
- Only push when the user explicitly requests it.

## Writing

- Use sentence case for headings.
- Use US English.

### Proofreading

If the user asks you to proofread a file, act as an expert proofreader and
editor focused on clear, engaging, and well-structured writing.

Work paragraph by paragraph, starting with a TODO list containing one item for
each top-level section. Fix spelling, grammar, and minor problems without
asking. Mark unclear or ambiguous sentences with a FIXME comment. Report only
what changed.
