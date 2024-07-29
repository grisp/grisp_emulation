# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.2] - 2024-06-29

### Fixed

- Fix race condition in EEPROM emulation

## [0.2.1] - 2024-06-29

### Added

- Add EEPROM emulation

### Fixed

- Fix GPIO emulation bug

## [0.2.0] - 2023-03-20

### Added

- Add GRISP 2 support

### Changed

- New emulation's design and refactoring [grisp_emulation/#2](https://github.com/grisp/grisp_emulation/pull/2)

## [0.1.2] - 2020-07-02

### Fixed

- Fixed emulation of PmodGYRO to match the latest implementation in the GRiSP
  runtime ([grisp/\#76])

## [0.1.1] - 2020-03-03

### Fixed

- Add missing indices to the emulation GPIO driver ([\#1])

## [0.1.0] - 2019-03-21

### Added

- Initial release

[Unreleased]: https://github.com/grisp/grisp_emulation/compare/0.2.2...HEAD
[0.2.2]: https://github.com/grisp/grisp_emulation/compare/0.2.1...0.2.2
[0.2.1]: https://github.com/grisp/grisp_emulation/compare/0.2.0...0.2.1
[0.2.0]: https://github.com/grisp/grisp_emulation/compare/0.1.2...0.2.0
[0.1.2]: https://github.com/grisp/grisp_emulation/compare/v0.1.1...0.1.2
[0.1.1]: https://github.com/grisp/grisp_emulation/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/grisp/grisp_emulation/releases/v0.1.0

[grisp/\#76]: https://github.com/grisp/grisp/issues/76
[\#1]: https://github.com/grisp/grisp_emulation/pull/1
