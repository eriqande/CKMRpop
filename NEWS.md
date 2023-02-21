# CKMRpop 0.1.4.9999

* Fixed an error that occurs when no pairs are present, by returning a tibble
with the columns expected from `compile_related_pairs()` but 0 rows. Made the
same fix in `downsample_pairs()`.  Thanks to Martin Sköld for filing GitHub issue #2
about this.
* Removed tidyverse from the Suggests.
* Added GitHub Actions for checking and pkgdown


# CKMRpop 0.1.3

* Compiles on Solaris.
* Compatible with Rcpp 1.0.7.  (0.1.2 suffered a segfault with the new Rcpp).

# CKMRpop 0.1.2

* This was on CRAN for a short while until a compiler issue on Solaris caused
it to be archived.
