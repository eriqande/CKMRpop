#' compute the probability that a pair are half siblings
#'
#' Just a quick function for some developmental work.  This is not meant
#' for prime time (and, in fact, most of the inference related stuff like this
#' is going to be in a differnet package).
#' Depends on population sizes and covariates. This is not really
#' vectorized, but if you pass it multiple N values, it will return
#' a vector, one for each N value. Note that we only expect this to
#' work well for individuals with different birth years.  When they
#' have the same birth year they could have been full siblings and
#' also variance in reproductive success comes more into play. This
#' assumes an equal sex ratio.
#' @param y the difference in birth years
#' @param a1 the age at sampling of one member of the pair
#' @param a2 the age at sampling of another member of the pair
#' @param N total number of 2+ individuals in the population. This
#' can be a vector.
#' @param prob_repro The age-specific (starting from 1) probability of reproducing in a year
#' @param rel_repro The relative fecundities/reproductive potential of the individuals
#' @param surv_probs a vector giving the age specific survival probabilities.
#' @export
half_sib_kin_probs <- function(
  y,
  a1,
  a2,
  N,
  surv_probs,
  prob_repro,
  rel_repro
) {

  # compute the RERO's. We have to reconstitute a spip input list to pass to the leslie func. Lame.
  SL <- list()
  SL[["max-age"]] <- length(surv_probs)
  SL[["fem-surv-probs"]] <- surv_probs
  SL[["fem-prob-repro"]] <- prob_repro
  SL[["fem-asrf"]] <- rel_repro
  SL[["male-surv-probs"]] <- surv_probs
  SL[["sex-ratio"]] <- 0.5

  # stable age distribution
  SAD <- leslie_from_spip(SL, 1000)$stable_age_distro_fem
  # there is some jiggering to do to make sure the year classes line up with
  # the way we have parameterized reproductive success, etc.
  SAD <- SAD[-1] # tweeze off the 0 class
  SAD <- c(SAD, SAD[length(SAD)] * surv_probs[length(surv_probs)])
  SAD <- SAD / sum(SAD)


  RERO_1 <- prob_repro * rel_repro
  RERO_1 <- RERO_1 / sum(RERO_1)



  # Now, the probability of a being a mother from a given age class is the
  # relative reproductive output of each age class, which has to take account
  # of the stable age distribution, like this:
  RERO_AC <- RERO_1 * SAD / sum(RERO_1 * SAD)


  # I am going to store all the age-specific component parts in a tibble
  # to make it easier to look at them while coding/debugging.
  bits <- tibble(
    age = 1:length(RERO_1),
    rero_1 = RERO_1,
    rero_ac = RERO_AC
  )

  # now, we can add the probs of mom surviving y years after the first birth
  bits2 <- bits %>%
    mutate(
      ma_surv_y = assp(y, surv_probs)
    )

  # very good, now, let's cycle over different n values:
  matchy_probs <- lapply(N, function(n) {
    bits2 %>%
      mutate(kin_prob = lead(rero_1, y, default = 0) / sum(rero_1 * sad_fem_counts(n, SAD))) %>%
      summarise(tot_prob = sum(rero_ac * ma_surv_y * kin_prob)) %>%
      pull(tot_prob)
  }) %>% unlist()

  # And, now, in the end, we have to multiply each one of these by the probability that
  # each individual were sampled when they were
  age_samp_prob <- SAD[a1] * SAD[a2] * (2 - (a1 == a2)) / (sum(SAD[2:4]) ^ 2)

  fem_probs <- matchy_probs

  # and at the very end, multiply by 2 for the two different sexes
  2 * fem_probs

}


