# Changelog for `excalibur`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- Changed default commit range from `HEAD` to `HEAD~1..HEAD`.
- Changed return code to failure if any checks failed during the run.
- Changed config file resolution to be relative to repository under check.
- Added expansion of variables `${filename}` and `${commit-range}` in check commands

## 0.1.0 - 2025-10-07

Initial release.
