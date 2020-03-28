## Test environments
* local OS X install, R 3.6.2
* ubuntu 14.04 (on travis-ci), R 3.6.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* The note that flags is because this package writes to the global environment. This is the goal of the package as it allows the user to troubleshoot reactive shiny code locally rather than having to launch the app. This behavior is mentioned several times in the documenation and README and there is a prompt for the user to acknowledge prior to executing the function.
