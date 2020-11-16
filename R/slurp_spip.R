
#' Read in the pedigree, census, and sampling information from the spip run
#'
#' This version assumes that the spip output has been processed into
#' three files: `spip_pedigree.tsv`, `spip_prekill_census.tsv`, and
#' `spip_samples.tsv`, inside the directory where `run_spip()` was run.
#' @param dir  the path to the directory where spip was run.  This is
#' returned by `run_spip()`.
#' @param num_generations how many generations back do you wish to consider
#' for find relatives of
#' each sampled individual.  0 means just the individual themselves (so, not very
#' interesting, and you likely wouldn't ever use it. 1 means up to
#' and including the parents; 2 means up to and including the grandparents; 3 means up to
#' and including the great grandparents; and so forth.
#' @return A list of tibbles:
#' - `pedigree`:
#' - `census`:
#' - `samples`:
#'
#' @export
slurp_spip <- function(
  dir,
  num_generations
) {

  ped_file <- file.path(dir, "spip_pedigree.tsv")
  census_file <- file.path(dir, "spip_prekill_census.tsv")
  sample_file <- file.path(dir, "spip_samples.tsv")


  # read in the files, and do any necessary processing
  ped <- vroom::vroom(
    file = ped_file,
    delim = "\t",
    col_types = "iccc"
  )
  census <- vroom::vroom(
    file = census_file,
    delim = "\t",
    col_types = "iiii"
  )
  samples <- vroom::vroom(
    file = sample_file,
    delim = "\t",
    col_types = "cc"
  ) %>%
    mutate(
      samp_years_list = str_split(syears, "  *"),
      samp_years_list = map(.x = samp_years_list, .f = function(x) as.integer(x))
    ) %>%
    select(-syears) %>%
    extract(
      ID,
      into = c("sex", "born_year"),
      regex = "^([MF])([0-9]+)_[0-9]+",
      remove = FALSE,
      convert = TRUE
    ) #%>%  # now, here we add a list column with the ancestors over num_generations
    #  mutate(
    #    ancestor_list = ancestor_vectors(
    #      indivs = ID,
    #      ped = ped,
    #      num_generations = num_generations + 1
    #    )
    #  )
    # The above implementation is too slow for millions of individuals


  # Now, get a data frame of the samples' ancestor and relatives and
  # join it to samples.
  SAR <- find_ancestors_and_relatives_of_samples(P = ped, S = samples$ID, num_generations)

  list(
    pedigree = ped,
    census = census,
    samples = left_join(samples, SAR, by = c("ID" = "sample_id"))
  )

}
