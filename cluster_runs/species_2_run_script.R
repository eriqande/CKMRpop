
## WARNING, BEFORE PRODUCTION RUNS, SET SEED WITH JOB AND TASK ARRAY IDS

# This is our cohort size that gives us a stable age distribution
# and population size of 2+ individuals of about half a million

# here are default values for testing.
cohort_size <- 3700
samp_frac <- 0.04
down_samp_n <- 400
slurm_job_id <- 37

#  expect a single argument from Rscript which is the array_idx
args = commandArgs(trailingOnly = TRUE)

if(length(args) != 4) {
  stop("Must be called from Rscript with the following parameters:
  - CohortSize: Take desired pop size and multiply by 370,000/500,000 to get the
    desired size of the populationof 2+ fish.
  - SampFrac: Fraction of 2, 3, and 4 year olds to sample in two years.  0.009 is a good
    value for pop size of 500,000.  So, scale accordingly.
  - DownSamp: the total sample size that it should be downsampled to, like 10,000, to
    get roughly two samples of size 5000 in each year.
  - SLURM_JOB_ID: The unique slurm JOB_ID used to set seeds.
")
} else {
  cohort_size <- as.integer(args[1])
  samp_frac <- as.numeric(args[2])
  down_samp_n <- as.integer(args[3])
  slurm_job_id = as.integer(args[4])
  file_str <- paste0(
    "cz_",
    cohort_size,
    "_sf_",
    samp_frac,
    "_dsn_",
    down_samp_n,
    "_sji_",
    slurm_job_id,
    ".rds"
  )
}


library(tidyverse)
library(CKMRpop)
SPD <- species_2_life_history


#### POP SIZE ####
# before we tell spip what the cohort sizes are, we need to
# tell it how long we will be running the simulation
SPD$`number-of-years` <- 60  # run the sim forward for 50 years


# Do some matrix algebra to compute starting values from the
# stable age distribution:
L <- leslie_from_spip(SPD, cohort_size)
sum(L$stable_age_distro_fem[-(1:2)])

# then we add those to the spip parameters
SPD$`initial-males` <- floor(L$stable_age_distro_fem)
SPD$`initial-females` <- floor(L$stable_age_distro_male)

# tell spip to use the cohort size
SPD$`cohort-size` <- paste("const", cohort_size, collapse = " ")


# how many 2, 3, and 4 year olds?
sum(L$stable_age_distro_fem[2:4]) + sum(L$stable_age_distro_male[2:4])


#### SAMPLING ####

SPD$`lethal-sampling` <- 1.00

samp_start_year <- 51
samp_stop_year <- 52
SPD$`discard-all` <- 0
SPD$`gtyp-ppn-fem-pre` <- paste(
  samp_start_year, "-", samp_stop_year,
  " 0 ",  # don't sample any 0/1 year-olds
  samp_frac, " ", samp_frac, " ", samp_frac, " ", # sample 1.5% of the 2, 3, and 4, year olds
  paste(rep(0, SPD$`max-age` - 4), collapse = " "),
  sep = ""
)
SPD$`gtyp-ppn-male-pre` <- SPD$`gtyp-ppn-fem-pre`


#### RUNNING SPIP ####

set.seed(slurm_job_id)
spip_dir <- run_spip(pars = SPD)

#### SLURPING SPIP ####
# now read that in and find relatives within the grandparental range
slurped <- slurp_spip(spip_dir, 2)


#### Get Size of Population 2 years and over ####

pop_sizes <- slurped$census_prekill %>%
  filter(age > 1) %>%
  group_by(year) %>%
  summarise(
    male_pop = sum(male),
    fem_pop = sum(female),
    tot_pop = male_pop + fem_pop
  )


#### ANALYZING THE PAIRWISE RELATIONSHIPS ####

samps <- slurped$samples %>%
  mutate(
    first_samp = map_int(samp_years_list, function(x) x[1]),  # just record the first year they were sampled
    age = first_samp - born_year
  ) %>%
  count(first_samp)


full_pairs <- compile_related_pairs(slurped$samples)

if(sum(samps$n) > down_samp_n) {
  dsp_list <- downsample_pairs(
    S = slurped$samples,
    P = full_pairs,
    n = down_samp_n
  )
}



relat_counts <- count_and_plot_ancestry_matrices(dsp_list$ds_pairs)



set.seed(22)
spag <- uncooked_spaghetti(
  Pairs = dsp_list$ds_pairs,
  Samples = dsp_list$ds_samples
)

# now, prepare a list to write out
ret_list <- list(
  cohort_size = cohort_size,
  samp_frac = samp_frac,
  down_samp_n = down_samp_n,
  slurm_job_id = slurm_job_id,
  file_str = file_str,
  pop_sizes = pop_sizes,
  full_samples = slurped$samples,
  full_pairs = full_pairs,
  ds_samples = dsp_list$ds_samples,
  ds_pairs = dsp_list$ds_pairs,
  ds_relat_counts = relat_counts,
  ds_spag = spag
)

write_rds(
  ret_list,
  path = file_str,
  compress = "xz"
)
