

#' Return a Leslie-like matrix from the spip parameters
#'
#' Here we take the survival rates for females and males and the sex ratio,
#' as well as the annual new cohort size (assumed constant),
#' and we make a leslie-like matrix
#' to compute the stable age distribution.
#'
#' For growing or shrinking populations, this is currently correct only when
#' the male and female survival rates are the same, and hence the stable
#' age distributions of the two sexes are the same as well.
#' @param P a named list of the spip parameters.
#' @param C the constant size of the newborn cohort each year
#' @param growth_rates If not NULL, a vector of desired growth rates.
#' For each one of these the function will return a leslie matrix, the stable
#' age distribution, the initial number of males and females at that
#' age distribution, and also the cohort sizes until time T (if T is not NA)
#' @param T the number of years to compute the cohort sizes for in the growing
#' (or shrinking) populations.
#' @return
#' This function returns a list with the following components:
#' * `stable_age_distro_fem`: a vector of the expected number of females in each age
#' group of 0 up to MaxAge-1 once the stable age distribution has been reached.  Note that
#' this corresponds to the PREKILL_CENSUS from spip. In this vector, the size of the
#' MaxAge group is left out because this is how it is needed to be to insert into
#' the `--initial-males` and `--initial-females` options in spip().  If you want the
#' size of all age classes, use the output list component
#' `stable_age_distro_fem_with_max_age_class`, described below.
#' * `stable_age_distro_male`: same as above, but for males.
#' * `stable_age_distro_fem_with_max_age_class`: The expected number of females from age 0 to
#' MaxAge once the stable age distribution has been reached.
#' * `stable_age_distro_male_with_max_age_class`: same as above, but for females.
#' * `female_leslie_matrix`: The Leslie matrix implied by the spip parameters in P.
#' @export
#' @examples
#' result <- leslie_from_spip(species_1_life_history, 300)
#'
#' # print the result list:
#' result2 <- leslie_from_spip(
#'   species_1_life_history,
#'   300,
#'   growth_rates = c(-.02, 0, 0.04),
#'   T = 60
#'  )
#'
#' # With several growth rates, including 0.
#'
leslie_from_spip <- function(P, C, growth_rates = NULL, T = NA) {

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



  # finally, for the females, we want to return what
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
  eig <- eigen(f_leslie)
  sad <- eig$vectors[,1] / eig$vectors[1,1]

  # we will return that proper leslie matrix of a constant sized population
  ret$female_leslie_matrix <- f_leslie

  # Now, if the user also requested leslie matrices, stable age distributions,
  # and cohort sizes for other growth rates, we can fulfill them here.
  # I expect that this could be solved more directly with some linear algebra,
  # but I am going to do it numerically.
  # In short, if the desired increase rate is r, then we want to find
  # a multiplier for all the fecundities in f_leslie that give an eigenvalue
  # of 1 + r.

  # So, first, here is a function of that multiplier, m, and f_leslie, and r, which returns
  # the absolute difference between r and the dominant eigenvalue.
  dom_eig_diff <- function(m, r) {
    M <- f_leslie
    M[1, ] <- M[1, ] * m
    e <- eigen(M)
    d <- as.numeric(e$values[1])
    abs(d - (1 + r))
  }

  # find the multiplier for each growth_rate and return them in a list
  if(!is.null(growth_rates)) {

    if(is.na(T)) stop("When growth_rates is not NULL, you must supply a value for T (number of years) ")

    optim_list <- lapply(growth_rates, function(x) optim(1, dom_eig_diff, r = x, method = "Brent", lower = 0.00001, upper = 1e5)$par)
    names(optim_list) <- growth_rates

    # now we cycle over thase fecundity fractions and we:
    # 1. find the stable age distribution
    # 2. find the cohort sizes for times MA + 1 to T + MA + 1
    # 3. format those cohort sizes for spip
    # then we return all that in a list.
    ret$growth_rate_results <- lapply(optim_list, function(x) {
      rrr <- list()  # for returning values
      f_leslie2 <- f_leslie
      f_leslie2[1,] <- f_leslie[1, ] * x
      eig2 <- eigen(f_leslie2)
      leading_eig2 <- eig2$values[1]
      sad2 <- as.numeric(eig2$vectors[,1] / eig2$vectors[1,1])
      #sad2 <- as.numeric(sad2 / sum(sad2))

      # at this point, sad2_f is the initial number of females of each age, assuming
      # that there are 300 newborns total
      sad2_f <- (1 - sr) * C * sad2
      sad2_m <- sr * C * sad2


      # We get set up to return these
      rrr$stable_age_distro_fem <- sad2_f[-length(sad2_f)]
      rrr$stable_age_distro_male <- sad2_m[-length(sad2_m)]

      # also return what the full stable age distro is
      rrr$stable_age_distro_fem_with_max_age_class <- sad2_f
      rrr$stable_age_distro_male_with_max_age_class <- sad2_m

      # that gives us the starting population sizes for the beginning
      # of the simulation, but now we need the number of males and females
      # entering the population in the ensuing T years. We do that by iterating
      # the Leslie matrix T years forward.
      N_mat <- matrix(NA, nrow = length(sad2_f), ncol = T)   # this is for holding the results
      N_mat[,1] <- f_leslie2 %*% sad2_f
      for(t in 2:T) {
        N_mat[,t] <- f_leslie2 %*% N_mat[, t - 1]
      }
      # the first row of N_mat now gives us the number of females entering the population
      # each year. So the total number of individuals entering the population will
      # be that value divided by the fraction of females.
      cohorts <- N_mat[1, ] / (1 - sr)
      cohorts_i <- round(cohorts)
      # now, we just return that, both as a numeric, as an int, and as a string
      # that can be used for the cohort size param
      rrr$cohorts_numeric <- cohorts
      rrr$cohorts_integer <- cohorts_i
      tmp <- paste(cohorts_i, collapse = " ")
      rrr$cohort_size_param_string <- paste("var", tmp, collapse = " ")

      rrr
    })

  }

  ret
}
