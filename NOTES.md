# Implementation Notes

General principles:

- Separate intermediate representations from passes. Define an SML module
  (`.sig` and `.sml` file) for the representation, another for the pass.
