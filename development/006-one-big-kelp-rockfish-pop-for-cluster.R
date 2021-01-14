library(CKMRpop)
library(tidyverse)


#' @param N the desired number of mature fish
#' @param prf the fem-prob-repro of the population
#' @param prm the male-prob-repro of the population
set_cohorts_and_inits <- function(N, pars) {
  # first check the stable age distribution for N = 1000
  check_it <- leslie_from_spip(pars, 1000)

  # and count up the number of mature females and males
  # in the population.  We partially count individuals of
  # different ages according to the prob of reproducing
  size <- sum(check_it$stable_age_distro_fem * pars$`fem-prob-repro`) +
    sum(check_it$stable_age_distro_male * pars$`male-prob-repro`)

  # now, scale the cohort size that would give us N
  cz <- N / (size / 1000)

  # now, use that to get the stable age distro and the init cohort sizes
  stable <- leslie_from_spip(pars, cz)

  # recheck for fun
  stable_size <- sum(stable$stable_age_distro_fem * pars$`fem-prob-repro`) +
    sum(stable$stable_age_distro_male * pars$`male-prob-repro`)

  # rescale again:
  cz2 <- N / (stable_size / cz)

  final <- leslie_from_spip(pars, cz2)

  final_size <- sum(final$stable_age_distro_fem * pars$`fem-prob-repro`) +
    sum(final$stable_age_distro_male * pars$`male-prob-repro`)

  # return a list
  ret <- list()
  ret$`initial-males` <- floor(final$stable_age_distro_male)
  ret$`initial-females` <- floor(final$stable_age_distro_fem)

  # tell spip to use the cohort size
  ret$`cohort-size` <- paste("const", ceiling(cz2), collapse = " ")

  ret
}



# make a set of spip pars for a single populations/deme that will
# have a stable pop size of N mature individuals. And then we
# have sampling fractions each year of recruits and adults.
# we assume that "adults" that we might sample are 5 years and up.
spip_pars_one_pop <- function(N, juv_samp_fract, adult_samp_fract, Y = 80) {
  pars <- list()
  pars$`max-age` <- 25
  pars$`number-of-years` = Y
  pars$`fem-prob-repro` <- c(0, 0, 0, 0.2, 0.5, 0.7, 0.9, 1.0, rep(1.0, 25 - 8))
  pars$`male-prob-repro` <- c(0, 0, 0, 0.2, 0.5, 0.7, 0.9, 1.0, rep(1.0, 25 - 8))
  pars$`fem-surv-probs` <- c(1, 1, 1, 0.5, 0.56, 0.62, 0.68, 0.74, 0.8, rep(0.85, 25 - 9))
  pars$`male-surv-probs` <- pars$`fem-surv-probs`
  pars$`fem-asrf` <- c(0, 0, 0, seq(4,25))
  pars$`male-asrp` <- pars$`fem-asrf`
  pars$`offsp-dsn` <- "negbin"
  pars$`fem-rep-disp-par` <- 0.25
  pars$`male-rep-disp-par` <- 0.25
  pars$`sex-ratio` <- 0.5
  pars$`mate-fidelity` <- 0.15 # some multiple paternity


  tmp <- set_cohorts_and_inits(N, pars)
  pars <- c(pars, tmp)

  pars$`discard-all` <- 0

  pars$`lethal-sampling` <- 1
  pars$`gtyp-ppn-fem-pre` <- c("75-77", juv_samp_fract, 0, 0, 0, rep(adult_samp_fract, 21))
  pars$`gtyp-ppn-male-pre` <- pars$`gtyp-ppn-fem-pre`

  pars
}


# the total estimated pop size is 410,958 fish.  So, about half a million
totPopSize <- 410958

# the sampling is set up to be about 2295 juveniles and 945 adults each year for three years

# get the expected stable age distribution
one_pop <- spip_pars_one_pop(totPopSize, 0.01, 0.02)

# from that, it looks like we would get our sample sizes with a juvenile
# numbers with a sampling fraction of this:
jsf <- 2295 / (2 * one_pop$`initial-males`[1])
jsf

# and we would get our adult sampling numbers with
asf <- 945 / (2 * sum(one_pop$`initial-males`[-(1:4)]))
asf

# so now, put it in there with the right sampling fractions
one_pop2 <-  spip_pars_one_pop(totPopSize, jsf, asf)

big_run <- run_spip(one_pop2)


# about 9600 samples when I ran it (seed not specified)
krf_slurped_2gen <- slurp_spip(big_run, 2)
slurped <- krf_slurped_2gen

# check the pop sizes:
g <- ggplot_census_by_year_age_sex(slurped$census_prekill)
ggsave(g, filename = "census.pdf", width = 10, height = 10)

crel <- compile_related_pairs(slurped$samples)
