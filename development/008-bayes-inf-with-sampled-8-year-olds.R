# Bayesian Inference of pop size including sampled indivs up to 8 years old"

# load libraries and source some functions
library(tidyverse)
library(CKMRpop)
source("development/R/half_sib_kin_probs.R")


# Since we are doing this on a bunch of new scenarios, I want to wrap more of this
# up into one big function,


#### HERE ARE SOME SUB FUNCTIONS USED BELOW ####
# here is a function to count them all up for a single simulation
count_classes <- function(x) {
  x$ds_pairs %>%
    mutate(
      syear1 = map_int(samp_years_list_1, function(x) x[1]),
      syear2 = map_int(samp_years_list_2, function(x) x[1]),
      sage1 = syear1 - born_year_1,
      sage2 = syear2 - born_year_2, # now make a parallel-ordered version of those
      osage1 = pmin(sage1, sage2),
      osage2 = pmax(sage1, sage2),
      birth_diff = abs(born_year_2 - born_year_1),
      relat_str = str_c(dom_relat, "-", max_hit)
    ) %>%
    filter(
      relat_str %in% c("Si-1", "A-2", "GP-1", "PO-1", "PO-2")
    ) %>%
    count(birth_diff, osage1, osage2, relat_str) %>%
    pivot_wider(
      names_from = relat_str,
      values_from = n,
      values_fill = 0L
    )
}

# sp is a vector of survival probs, the first is the prob of surviving from 0 to 1.
assp <- function(y, sp) {
  if(y == 0) return(rep(1, length(sp)))

  # fiddle the survival probs to get them to reflect 1-year-olds
  sp2 <- sp[-1]
  sp2 <- c(sp2, rep(0, 300))  # get the zero probability for going beyond the max age

  lapply(1:length(sp), function(x) prod(sp2[x:(x+y-1)])) %>%
    unlist()

}

sad_fem_counts <- function(N, SAD) {
  two_plus <- N * SAD[-1] / sum(SAD[-1])
  ones <- N * SAD[1] / sum(SAD[-1])
  c(ones, two_plus) / 2   # divide by two because we are just counting females
}



#' One big function to operate on a directory full of simulation results
#'
#' from the simulation results (named cz...) in directory dir, run through all the
#' inference steps
infer_pop_sizes <- function(dir = "cluster_runs_samp1-8_5000_total") {

  ####  READ THE DATA IN ####
  files <- dir(
    path = dir,
    pattern = "cz_.*\\.rds",
    full.names = TRUE
  )
  names(files) <- basename(files)

  output_list <- lapply(files, function(x) {
    R <- read_rds(x)
    R$ds_pairs <- R$ds_pairs %>%
      mutate(
        samp_years_list_1 = samp_years_list_pre_1,
        samp_years_list_2 = samp_years_list_pre_2,
      )
    R$ds_samples <- R$ds_samples %>%
      mutate(
        samp_years_list = samp_years_list_pre
      )
    R
  })


  # and here we count them up and store them in a list column.
  # Note that we will filter these down to different birth diffs
  # at some later time.
  pair_cat_counts <- tibble(
    sim_id = names(output_list),
    cohort_size = map_chr(output_list, function(x) x$cohort_size),
    pair_categ_tibs = map(output_list, count_classes)
  ) %>%
    mutate(sim_num = 1:n())


  #### COUNT THE NUMBER OF PAIRS OF DIFFERENT AGE CATEGORIES, ETC ####
  # a function to count the total number of pairwise comparisons
  compute_sample_pairs <- function(x) {
    s1 <- x$ds_samples %>%
      mutate(
        syear = map_int(samp_years_list, ~.x[1]),
      ) %>%
      count(born_year, syear) %>%
      mutate(id = 1:nrow(.)) %>%
      select(id, born_year, syear, n)

    s2 <- s1
    names(s1) <- str_c(names(s1), "1")
    names(s2) <- str_c(names(s2), "2")

    s3 <- bind_cols(
      s1[rep(1:nrow(s1), each = nrow(s1)), ],
      s2[rep(1:nrow(s2), times = nrow(s2)), ]
    ) %>%
      filter(id1 <= id2) %>% # don't count these pairwise groups more than once
      mutate(
        n_pairs = case_when(
          id1 == id2 ~ n1 * (n1 - 1) / 2,
          TRUE ~ n1 * n2 * 1
        ),
        birth_diff = abs(born_year2 - born_year1),
        age1 = syear1 - born_year1,
        age2 = syear2 - born_year2,
        osage1 = pmin(age1, age2),
        osage2 = pmax(age1, age2)
      ) %>%
      group_by(birth_diff, osage1, osage2) %>%
      summarise(tot_pairs = sum(n_pairs), .groups = "drop")

    s3
  }

  # then get a tibble of them:
  tot_pair_counts <- tibble(
    sim_id = names(output_list),
    cohort_size = map_chr(output_list, function(x) x$cohort_size),
    tot_pair_tibs = map(output_list, compute_sample_pairs)
  ) %>%
    mutate(sim_num = 1:n())



  #### STORE A QUICK CHECK ON THE TOTAL NUMBER OF PAIRS ####

  TPC_summary <- tot_pair_counts %>%
    unnest(tot_pair_tibs) %>%
    group_by(sim_num) %>%
    summarise(all_pairs = sum(tot_pairs))

  #### JOIN TOTAL PAIRS ONTO OBSERVED NUMBERS ####

  pcc_u <- pair_cat_counts %>%
    unnest(pair_categ_tibs)

  tpc_u <- tot_pair_counts %>%
    unnest(tot_pair_tibs)

  all_pairs_n <- full_join(
    pcc_u,
    tpc_u
  ) %>%
    replace_na(data = ., replace = list(`GP-1` = 0, `A-2` = 0, `A-1` = 0, `Si-1` = 0, `PO-1` = 0)) %>%
    mutate(
      totKin = `Si-1` + `A-2` + `GP-1`,
      `fractNonSi-1` = (`A-2` + `GP-1`) / totKin
    )


  # let's record the number of all these different types of pairs for birth_diffs = 0 or not
  tot_relat_counts <- all_pairs_n %>%
    select(-tot_pairs, -totKin, -`fractNonSi-1`) %>%
    pivot_longer(
      cols = `Si-1`:`GP-1`,
      names_to = "relat",
      values_to = "num"
    ) %>%
    mutate(same_birth_year = birth_diff == 0) %>%
    group_by(sim_num, sim_id, cohort_size, same_birth_year, relat) %>%
    summarise(num_pairs = sum(num)) %>%
    ungroup()

  ### The distribution of A-2 and GP-1 relative to Si-1

  # make a plot to see how these differ over different age groups
  sib_vs_aunt_gp_plot <- all_pairs_n %>%
    mutate(categ = str_c(birth_diff, osage1, osage2, sep = "-")) %>%
    ggplot(aes(x = totKin, y = `fractNonSi-1`, fill = factor(cohort_size))) +
    geom_point(shape = 21, stroke = 0.2, size = 2) +
    facet_wrap(~ categ, ncol = 4)

  # compute the fraction of non-Sib kins with the same degree of genome sharing
  NSIF <- all_pairs_n %>%
    filter(birth_diff > 0) %>%
    summarise(nonSi_fract = sum(`A-2` + `GP-1`) / sum(totKin)) %>%
    pull(nonSi_fract)

  # Note that with a large span of age categories, this will not be accurate---it should
  # probably be more age-difference specific...



  ####  COMPUTING AND CHECKING THE HALF-SIB KIN-PAIR PROBS ####

  # first, find all the different categories
  pair_cats <- all_pairs_n %>%
    distinct(cohort_size, birth_diff, osage1, osage2) %>%
    mutate(pop_size = case_when(
      cohort_size == "2220000" ~ 3.0e6,
      cohort_size == "1110000" ~ 1.5e6,
      cohort_size == "740000" ~ 1.0e6,
      cohort_size == "370000" ~ 0.5e6,
    )) %>%
    select(cohort_size, pop_size, birth_diff, osage1, osage2)

  # calculate the half-sib probs
  calced_probs <- pair_cats %>%
    mutate(
      hs_prob = pmap_dbl(
        .l = list(
          y = birth_diff,
          a1 = osage1,
          a2 = osage2,
          N = pop_size),
        .f = function(y, a1, a2, N) {
          half_sib_kin_probs(
            y = y,
            a1 = a1,
            a2 = a2,
            N = N,
            surv_probs = species_2_life_history$`fem-surv-probs`,
            prob_repro = species_2_life_history$`fem-prob-repro`,
            rel_repro = species_2_life_history$`fem-asrf`
          )
        }
      )
    )

  # Now, we can join those to the observed ones and see how they compare:
  comp <- all_pairs_n %>%
    left_join(calced_probs) %>%
    mutate(
      expected_Si1 = hs_prob * tot_pairs,
      categ_str = str_c(birth_diff, osage1, osage2, sep = "-")
    )


  # make a big old plot of that stuff
  hs_kin_pair_probs_vs_observed_fractions_plot <- comp %>%
    filter(birth_diff > 0) %>%
    ggplot(., aes(x = expected_Si1, y = `Si-1`, fill = categ_str)) +
    geom_point(shape = 21, stroke = 0.2) +
    geom_abline(intercept = 0, slope = 1) +
    facet_wrap(~ pop_size, ncol = 1)



  #### Doing the actual estimation ####

  Npts <- seq(5e4, 8e6, by = 5e4)
  Nprobs <- all_pairs_n %>%
    distinct(birth_diff, osage1, osage2) %>%
    mutate(
      hs_prob = pmap(
        .l = list(
          y = birth_diff,
          a1 = osage1,
          a2 = osage2
        ),
        .f = function(y, a1, a2) {
          half_sib_kin_probs(
            y = y,
            a1 = a1,
            a2 = a2,
            N = Npts,
            surv_probs = species_2_life_history$`fem-surv-probs`,
            prob_repro = species_2_life_history$`fem-prob-repro`,
            rel_repro = species_2_life_history$`fem-asrf`
          )
        }
      )
    ) %>%
    mutate(
      hs_tib = map(hs_prob, function(x) tibble(N = Npts, hs_prob = x))
    ) %>%
    select(-hs_prob) %>%
    unnest(cols = c(hs_tib))


  # Then, we will join the probabilities on there and we use the total kin corrected
  # by the non-Si-1 fraction and the tot_pairs to compute the contributions
  # to the log likelihood.  UPDATE FOR THE 8-year olds.  We don't have a, constant
  # non-half-sib fraction across all the different age categories.  If we had that
  # for each pair of ages (which we will be able to compute easily with CKMRretro
  # when I have written it) then we could use that.  BUT, for now, I just use the
  # observed number of half-siblings.

  # first get the logl terms from all of them (even birth_diff == 0 ones)
  logl_terms <- all_pairs_n %>%
    left_join(
      Nprobs,
      by = c("birth_diff", "osage1", "osage2")
    ) %>%
    mutate(
      logl_term_NSIF = (totKin * (1 - NSIF) * log(hs_prob)) +
        ((tot_pairs - totKin * (1 - NSIF)) * log(1 - hs_prob)),
      # this version just uses the half-sib pairs straight up, because the NSIF
      # is not reliable for compensating for A-2 and GP-1 in all age categories.
      # But we want to see how it would look if we could do that.
      logl_term = (`Si-1` * log(hs_prob)) +
        ((tot_pairs - `Si-1`) * log(1 - hs_prob))
    )

  # then, let's get the scaled likelihoods/posteriors out of those,
  # with birth_diff > 0
  scaled_likelihoods <- logl_terms %>%
    filter(birth_diff > 0) %>%
    group_by(sim_num, cohort_size, N) %>%
    summarise(
      logl = sum(logl_term),  # this is the sum over age and birth_diff categories of the binomial terms
      logl_NSIF = sum(logl_term_NSIF)
    ) %>%
    mutate(
      normo_logl = logl - max(logl),
      scaled_likelihood = exp(normo_logl) / sum(exp(normo_logl)),
      normo_logl_NSIF = logl_NSIF - max(logl_NSIF),
      scaled_likelihood_NSIF = exp(normo_logl_NSIF) / sum(exp(normo_logl_NSIF))
    ) %>%
    mutate(pop_size = case_when(
      cohort_size == "2220000" ~ 3.0e6,
      cohort_size == "1110000" ~ 1.5e6,
      cohort_size == "740000" ~ 1.0e6,
      cohort_size == "370000" ~ 0.5e6
    ))

  # while we are at it, let's filter these down to the maxes
  # as well, to throw a rug onto the plots

  like_maxes <- scaled_likelihoods %>%
    filter(normo_logl == 0)

  like_maxes_NSIF <- scaled_likelihoods %>%
    filter(normo_logl_NSIF == 0)



  # Now, we can plot those things, which are essentially posterior probability curves
  scaled_likelihoods_plots <- ggplot(
    scaled_likelihoods,
    aes(x = N, y = scaled_likelihood, colour =  factor(pop_size), group = sim_num)
  ) +
    geom_vline(aes(xintercept = pop_size)) +
    geom_line(size = 0.2) +
    geom_rug(data = like_maxes, aes(y = NULL)) +
    facet_wrap(~ pop_size, ncol = 1, scales = "free_y") +
    xlim(0, 8e6)

  scaled_likelihoods_plots_NSIF <- ggplot(
    scaled_likelihoods,
    aes(x = N, y = scaled_likelihood_NSIF, colour =  factor(pop_size), group = sim_num)
  ) +
    geom_vline(aes(xintercept = pop_size)) +
    geom_line(size = 0.2) +
    geom_rug(data = like_maxes_NSIF, aes(y = NULL)) +
    facet_wrap(~ pop_size, ncol = 1, scales = "free_y") +
    xlim(0, 8e6)


  #### COEFFICIENTS OF VARATION, ETC. ####

  # We can approach this from a Bayesian perspective first, calculating the
  # variance (and from that the standard deviation) of the posterior distribution, and dividing the
  # posterior mean by the standard deviation of the distribution.
  coeffs_of_v <- scaled_likelihoods %>%
    group_by(sim_num, pop_size) %>%
    summarise(
      posterior_mean = sum(N * scaled_likelihood),
      posterior_variance = sum( scaled_likelihood * ((posterior_mean - N) ^2) ),
      posterior_sd = sqrt(posterior_variance),
      coeff_of_variation = posterior_sd / posterior_mean
    )

  coeffs_of_v_NSIF <- scaled_likelihoods %>%
    group_by(sim_num, pop_size) %>%
    summarise(
      posterior_mean = sum(N * scaled_likelihood_NSIF),
      posterior_variance = sum( scaled_likelihood_NSIF * ((posterior_mean - N) ^2) ),
      posterior_sd = sqrt(posterior_variance),
      coeff_of_variation = posterior_sd / posterior_mean
    )


  # Averaged over the 20 runs we have:
  cv_averages <- coeffs_of_v %>%
    group_by(pop_size) %>%
    summarise(mean_cv = mean(coeff_of_variation))


  # Averaged over the 20 runs we have:
  cv_averages_NSIF <- coeffs_of_v_NSIF %>%
    group_by(pop_size) %>%
    summarise(mean_cv = mean(coeff_of_variation))


  # We can also look at the observed distribution of MLEs to estimate the mean-squared
  # error, and then take the square root of that.  And look at the ratio of the sqrt(MSE) to thetrue
  # value as another CV-like measurement.
  mse_of_mles <- like_maxes %>%
    group_by(pop_size) %>%
    summarise(
      MSE = mean( (pop_size - N) ^ 2),
      sqrt_MSE = sqrt(MSE),
      ratio = sqrt_MSE / pop_size[1]
    )


  #### RETURN WHAT WE WANT/NEED IN A BIG LIST ####
  list(
  	pair_cat_counts = pair_cat_counts,
  	tot_pair_counts = tot_pair_counts,
  	TPC_summary = TPC_summary,
  	all_pairs_n = all_pairs_n,
  	tot_relat_counts = tot_relat_counts,
  	sib_vs_aunt_gp_plot = sib_vs_aunt_gp_plot,
  	NSIF = NSIF,
  	hs_kin_pair_probs_vs_observed_fractions_plot = hs_kin_pair_probs_vs_observed_fractions_plot,
  	scaled_likelihoods = scaled_likelihoods,
  	like_maxes = like_maxes,
  	like_maxes_NSIF = like_maxes_NSIF,
    scaled_likelihoods_plots = scaled_likelihoods_plots,
  	scaled_likelihoods_plots_NSIF = scaled_likelihoods_plots_NSIF,
  	coeffs_of_v = coeffs_of_v,
  	coeffs_of_v_NSIF = coeffs_of_v_NSIF,
    cv_averages = cv_averages,
  	cv_averages_NSIF = cv_averages_NSIF,
  	mse_of_mles = mse_of_mles
  )

}


#### Now, go ahead and call that function on the three directories of output ####

total_outputs <- list()
i <- 0
for(D in c(
  "cluster_runs_pop3M_samp1-8_10K_and_7K_total/RDSs/pop3M_ds10000",
  "cluster_runs_pop3M_samp1-8_10K_and_7K_total/RDSs/pop3M_ds7000",
  "cluster_runs_pop3M_samp1-8_10K_and_7K_total/RDSs/pop3M_ds15000"
)) {
  i <- i + 1
  print(D)
  total_outputs[[i]] <- infer_pop_sizes(D)
}

names(total_outputs) <- c("ssz10000", "ssz7000", "ssz15000")

#write_rds(total_outputs, file = "total_outputs_pop3M.rds", compress = "xz")

