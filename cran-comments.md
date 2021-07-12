## Test environments

* win-builder (oldrelease, release, devel)
* local OS X install, R 4.0.3
* ubuntu 16.04.6 (on travis-ci), R 4.0.2
* CentOS Linux release 8.2.2004 (on my cluster), 4.0.3 
* Oracle Solaris 10, x86, 32 bit, R release, Oracle Developer Studio 12.6 (on rhub), R 4.1.0


## R CMD check results

WINDOWS OLDRELEASE, 4.0.5, win-builder: 0 errors | 0 warnings | 1 notes
  1 note = checking CRAN incoming feasibility (package archived)
WINDOWS RELEASE, 4.1.0, win-builder: 0 errors | 0 warnings | 1 notes
  1 note = checking CRAN incoming feasibility (package archived)
WINDOWS DEVEL, (2021-07-09 r80613), win-builder: 1 errors | 0 warnings | 1 notes
  1 note = checking CRAN incoming feasibility (package archived)
MAC, local: 0 errors | 0 warnings | 1 notes
  1 note =  installed size is 6.0Mb. sub-directories of 1 Mb or more: data 1.1 Mb, doc 3.5 Mb
LINUX, travis-ci: 0 errors | 0 warnings | 1 notes
  1 note = installed size is 6.9 Mb. sub-directories of 1 Mb or more: data 1.1 Mb, doc 2.7 Mb, libs 1.9 Mb, help 1.0 Mb
LINUX, CentOS: 0 errors | 0 warnings | 1 notes
  1 note = installed size is 8.7 Mb. sub-directories of 1 Mb or more: data 1.8, doc 2.9, help 1.1, libs 2.8
SOLARIS: 0 errors | 0 warnings | 3 notes:
  1 note = installed size is 6.1 Mb.
  1 note = checking CRAN incoming feasibility (package archived)
  1 note = "Files ‘README.md’ or ‘NEWS.md’ cannot be checked without ‘pandoc’ being installed."
      which seems due to the configuration of the build/test system.



## Downstream dependencies

Currently no known reverse dependencies

## User Notices

* Hello CRAN maintainers.  This was accepted to CRAN in late June, but then
I was notified by Prof. Brian Ripley that there were compiler errors on Solaris.
I discovered the cause of that was an ambiguous cast situation in the C++ code that
is not problematic on other compilers.  I fixed that and was waiting as long as
possible before resubmitting to allow all the other checks and build steps
you might have been doing
before resubmitting this.  When I went to test it again before final submission,
I found one of the examples causing an Error on some platforms.  I finally
tracked it down to an interaction between my code and some changes in RCpp 1.0.7,
which went up on CRAN just recently.  I managed to figure that issue out using valgrind,
and it was fairly easy to fix.   In the meantime, however, CKMRpop got archived on CRAN,
so this now looks like a new submission.  Sorry about all of that.

Thanks again for all that you do in support of the R community.



