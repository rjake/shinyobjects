## Test environments
* local R installation, R 3.6.3
* ubuntu 16.04 (on travis-ci), R 3.6.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This iteration returns more reactive objects than v 0.1.1 This version gives the user access to output$... objects and the results of eventReactive() and reactiveValues(). There is a bit of refactored code for more robust logic and bug fixes for the objects returned to the user.
