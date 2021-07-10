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
WINDOWS DEVEL, (2021-07-07 r80695), win-builder: 1 errors | 0 warnings | 1 notes
  1 note = checking CRAN incoming feasibility (package archived)
  1 error = in example for find_ancestors_and_relatives_of_samples(). This doesn't
    occur on any of my other test platforms. I don't know what the problem is here.
MAC, local: 0 errors | 0 warnings | 1 notes
  1 note =  installed size is 6.0Mb. sub-directories of 1 Mb or more: data 1.1 Mb, doc 3.5 Mb
LINUX, travis-ci: 0 errors | 0 warnings | 1 notes
  1 note = installed size is 6.9 Mb. sub-directories of 1 Mb or more: data 1.1 Mb, doc 2.7 Mb, libs 1.9 Mb, help 1.0 Mb
LINUX, CentOS: 0 errors | 0 warnings | 1 notes
  1 note = installed size is 8.7 Mb. sub-directories of 1 Mb or more: data 1.8, doc 2.9, help 1.1, libs 2.8


## Downstream dependencies

Currently no known reverse dependencies

## User Notices

* Hello CRAN maintainers.  This was accepted to CRAN in late June, but then
I was notified by Prof. Brian Ripley that there were compiler errors on Solaris.
I discovered the cause of that was an ambiguous cast situation in the C++ code that
is not problematic on other compilers.  I fixed that quickly 
by making a few explicit casts. I was letting all the other checks and things
happen before resubmitting this, and unfortunately that ran into my vacation, so this
did not get resubmitted before it was archived on CRAN.  Anyway, I hope it can be
re-instated somehow.  Sorry for the hassle. Thank you and best wishes.




