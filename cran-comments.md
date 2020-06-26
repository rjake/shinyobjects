## Test environments
* local R installation, R 3.6.2
* ubuntu 16.04 (on travis-ci), R 3.6.2
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

* Minor fixes to improve user experience and minimize the need to reference documentation. Currently convert_selection() will not work if the user doesn't specify the environment. This patch will prompt the user for the environment.
