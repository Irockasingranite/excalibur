# Excalibur

Excalibur runs your QA checks for you, in CI or locally.

* Manage all your checks in one place by creating an `excalibur.yaml` file:

```yaml
on-repository:
  - name: Run all tests
    command: invoke tests
    expected-exit: 0
  - name: Gitlint
    command: gitlint
    expected-exit: 0

on-commit:
  - name: Check formatting
    command: clang-format --dry-run -Werror
    expected-exit: 0
    files:
      - "**/**.c"
      - "**/**.h"
```

* Run checks on the final state of a PR, or on each individual commit.
* Run the same checks locally as in your CI pipeline.
* Checks run in a copy of your repository, so they don't mess with your working copy or litter it
  with their output.
