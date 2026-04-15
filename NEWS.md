# Tivy 0.1.1

## Bug Fixes & Internal Improvements

* Improved robustness of `dms_to_decimal()`:
  - Replaces problematic DMS symbols safely
  - Handles UTF-8/ASCII conversion explicitly
  - Enhanced validation and numeric component parsing
  - More informative warnings for malformed coordinates
  
* Fixed `process_pdf_text()` silently dropping coordinate pairs beyond
  the first in a block. Now iterates over all pairs, generating one
  row per zone.

# Tivy 0.1.0

## Initial Release

* Basic data import and processing capabilities
* Simple coordinate conversion functions
* Preliminary visualization tools
* Core spatial analysis features
