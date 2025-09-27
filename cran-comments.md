## Test environments
- Local Windows 10, R 4.1.3 (x86_64), RStudio
- win-builder: R-release, R-devel (OK)
- Vignettes build locally

## R CMD check results
0 errors | 0 warnings | 0 notes

## Release summary (1.1.2)
- Fixes critical bugs in batch calculation functions (see NEWS.md).
- Packaging clean-up: excluded accidentally committed `api/` tree and binary artifacts via `.Rbuildignore`.
- No compiled code; no external downloads during build/check.