## Resubmission

This is a resubmission. In this version I have:

* Fixed old GitHub URLs (301 redirects) in the package documentation.
* Added `submit_cran.R` to `.Rbuildignore` to remove non-standard
  top-level file from the tarball.

## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new submission.

* NOTE: Suggests or Enhances not in mainstream repositories: 'grofit'.
  The grofit method is an optional backend for growth parameter fitting;
  the default method (growthcurver) does not require grofit. When grofit
  is unavailable, the package gracefully informs the user.

## Test environments

* Windows 11 (local), R 4.1.2
* GitHub Actions (ubuntu-latest), R release
* GitHub Actions (ubuntu-latest), R devel

## Downstream dependencies

None (new package).
