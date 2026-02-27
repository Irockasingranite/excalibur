# Changelog for `excalibur`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- Fixed various issues in the requirements document and added missing specifications for check execution behaviour.
- Fixed on-repository checks being checked out in the original repository instead of the temporary copy.
- Fixed check failure report entries having the wrong keys for exit codes and missing the expected exit code.

## 0.1.1 - 2025-10-08

- Changed default commit range from `HEAD` to `HEAD~1..HEAD`.
- Changed return code to failure if any checks failed during the run.
- Changed config file resolution to be relative to repository under check.
- Added expansion of variables `${filename}` and `${commit-range}` in check commands

## 0.1.0 - 2025-10-07

Initial release.
