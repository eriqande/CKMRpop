library(tidyverse)

# this is just to make a job array control file
# we want to do samples in two consecutive years with
# a total of 10,000 fish, and roughly 5,000 in each.

# we want to do population sizes of half a million,
# 1 million, and 1.5 million.

# let's do 20 iterations of each.  I am going to request 50 Gb of
# RAM for each, and will make sure that no more than 20 run at a time.

# cohort sizes for the different pop sizes
CZ <- (370000/500000) * c(5e5, 1e6, 1.5e6)


# sampling fraction
SF <- round(0.009 / c(1, 2, 3), 5)

# Downsample number
DS <- rep(1e4, 3)


REPS <- 20

# Now make a tibble of the run parameters
tib <- tibble(
  CZ = rep(CZ, REPS),
  SF = rep(SF, REPS),
  DS = rep(DS, REPS)
) %>%
  mutate(index = 1:n()) %>%
  select(index, everything())


write_tsv(
  tib,
  path = "cluster_runs/array_jobs.tsv"
)
