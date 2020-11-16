#' Return a string of ancestor abbreviations in the order of the ancestor vectors
#'
#' Use this to put axis labels on plots, etc.
#' @param L desired length of the output
#' @export
ancestor_abbrvs <- function(L) {
  # determine how many generations to go (and go a bit over, possibly)
  g <- ceiling(log(L + 1, base = 2))

  alist <- list(
    "s",
    c("p", "m")
  )

  for(i in 3:g) {
    alist[[i]] <- paste0(rep(alist[[i-1]], each = 2), c("p", "m") )
  }

  unlist(alist)[1:L]
}
