## Test environments

* win-builder (oldrelease, release, devel)
* local OS X install, R 4.0.3
* ubuntu 16.04.6 (on travis-ci), R 4.0.2
* CentOS Linux release 8.2.2004 (on my cluster), 4.0.3


## R CMD check results

WINDOWS OLDRELEASE, 4.0.5, win-builder: 0 errors | 0 warnings | 1 notes
  1 note = checking CRAN incoming feasibility
WINDOWS RELEASE, 4.1.0, win-builder: 0 errors | 0 warnings | 1 notes
  1 note = checking CRAN incoming feasibility
WINDOWS DEVEL, (2021-06-14 r80502), win-builder: 0 errors | 0 warnings | 1 notes
  1 note = checking CRAN incoming feasibility
MAC, local: 0 errors | 0 warnings | 1 notes
  1 note =  installed size is 6.0Mb. sub-directories of 1 Mb or more: data 1.1 Mb, doc 3.5 Mb
LINUX, travis-ci: 0 errors | 0 warnings | 1 notes
  1 note = installed size is 6.8 Mb. sub-directories of 1 Mb or more: data 1.1 Mb, doc 2.7 Mb, libs 1.9 Mb
LINUX, CentOS: 0 errors | 0 warnings | 1 notes
  1 note = installed size is 8.7 Mb. sub-directories of 1 Mb or more: data 1.8, doc 2.9, help 1.1, libs 2.8


## Downstream dependencies

Currently no known reverse dependencies

## User Notices

* First submission to CRAN.
* Large installed size driven mostly by vignettes with plotted figures, and, on Linux, the compiled C++ libs.
