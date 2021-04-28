

#' Return a Leslie-like matrix from the spip parameters
#'
#' Here we take the survival rates for females and males and the sex ratio,
#' as well as the annual new cohort size (assumed constant),
#' and we make a leslie-like matrix
#' to compute the stable age distribution.
#' @param P a named list of the spip parameters.
#' @param C the constant size of the newborn cohort each year
#' @return
#' This function returns a list with the following components:
#' * `stable_age_distro_fem`: a vector of the expected number of females in each age
#' group of 0 up to MaxAge once the stable age distribution has been reached.  Note that
#' this corresponds to the PREKILL_CENSUS from spip.  If you remove the first element and
#' consider the stable age distribution of ages 1 to MaxAge, that would correspond to the
#' POSTKILL_CENSUS from spip.  Note that spip's values are random and those reported here
#' are expectations.
#' * `stable_age_distro_male`: same as above, but for males.
#' @export
leslie_from_spip <- function(P, C) {

  # prepare a return list
  ret <- list()

  # clean up the friggin names in P.  Once again, R doesn't do dashes well...
  names(P) <- gsub("\\p{Pd}", "-", names(P), perl = TRUE)

  # check to make sure that P has all the components we need
  needed  <- c(
    "max-age",
    "fem-surv-probs",
    "fem-prob-repro",
    "fem-asrf",
    "male-surv-probs",
    "sex-ratio"
    )
  needed <- gsub("\\p{Pd}", "-", needed, perl = TRUE)

  not_there <- setdiff(needed, names(P))
  if(length(not_there) > 0) {
    stop(
      "You don't have the following needed elements in P: ",
      paste(not_there, collapse = ", ")
    )
  }

  # first, get the values we need into convenient variables.
  A <- P[["max-age"]]
  fs <- P[["fem-surv-probs"]]
  pr <- P[["fem-prob-repro"]]
  ff <- P[["fem-asrf"]]
  ms <- P[["male-surv-probs"]]
  sr <- P[["sex-ratio"]]

  # now we make the matrix. We start by making a diagonal
  # matrix of s values, but we add an extra column onto that,
  # and then we slap a new row on the top.  The first value of
  # that row is a 1, which signifies that the cohort size next year
  # is exactly the same as it was the year before (which is how things
  # work with the "const" cohort size in spip).
  m1 <- matrix(0, nrow = length(fs), ncol = length(fs))
  diag(m1) <- fs
  m2 <- cbind(m1, 0.0)
  m3 <- rbind(0.0, m2)
  m3[1,1] <- 1.0

  # that gives us our matrix for females:
  MF <- m3

  # now, we do the same with the male survival probs
  m1 <- matrix(0, nrow = length(ms), ncol = length(ms))
  diag(m1) <- ms
  m2 <- cbind(m1, 0.0)
  m3 <- rbind(0.0, m2)
  m3[1,1] <- 1.0

  MM <- m3

  # now, we get the stable age distros:
  eig_f <- eigen(MF)$vectors[,1]
  sad_f <- (1 - sr) * C * eig_f / eig_f[1]

  eig_m <- eigen(MM)$vectors[,1]
  sad_m <- sr * C * eig_m / eig_m[1]

  # put those in the return list.  To make them conform to how spip
  # wants the values, take the last age class off of it.
  ret$stable_age_distro_fem <- sad_f[-length(sad_f)]
  ret$stable_age_distro_male <- sad_m[-length(sad_m)]

  # also return what the full stable age distro is
  ret$stable_age_distro_fem_with_max_age_class <- sad_f
  ret$stable_age_distro_male_with_max_age_class <- sad_m



  # finally, for the females, we want to return a what
  # a proper Leslie matrix would look like for a non-growing population
  # of this size. (i.e, we want to compute the fecundities that would go in the
  # top row of the actual leslie matrix).  This could be used to later fiddle with
  # so as to design spip to simulate growing populations, etc.
  # The way we do this is we figure out the relative fecundities of each age class
  # using the prop of reproducing and the age-specific relative fecundities, then
  # we scale these so that the expected number of offspring in the next cohort, given
  # the stable age distribution, is C.
  rel_f <- fs * pr * ff
  tmp <- sum(rel_f * sad_f[-length(sad_f)])
  f <- rel_f * (1 - sr) * C / tmp

  f_leslie <- MF
  f_leslie[1, ] <- c(f, 0.0)

  ret$female_leslie_matrix <- f_leslie

  ret
}
