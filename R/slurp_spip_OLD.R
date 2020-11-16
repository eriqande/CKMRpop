#' Read in spip output and return as a list of tibbles
#'
#' Bare bones at the moment.  This returns the full pedigree
#' and the census sizes.  It also returns a full list of the
#' samples, and also a tibble that includes the self, full-sib,
#' half-sib, and parent-offspring pairs amongst the samples, as
#' determined from the pedigree.
#' @param sfile the path to the spip outfile
#' @export
slurp_spip_OLD <- function(sfile) {
  S <- read_lines(sfile)

  # get the pedigree
  pfile <- tempfile()
  S[str_detect(S, "^PEDIGREE")] %>%
    cat(., sep = "\n", file = pfile)

  pedigree <- read_table2(
    pfile,
    col_types = "--i-ccc",
    col_names = c("year", "kid", "pa", "ma")
  )

  # get the census sizes and put them in a long tibble
  top_census_line <- S[min(which(str_detect(S, "^PREKILL_CENSUS_AGES")))]
  ages <- str_split_fixed(top_census_line, pattern = ":", n = 3)[,3] %>%
    str_split(., "\t") %>%
    .[[1]] %>%
    .[-1]

  cfile <- tempfile()
  S[str_detect(S, "^PREKILL_CENSUS_COUNTS")] %>%
    cat(., sep = "\n", file = cfile)
  census <- read_table2(
    cfile,
    col_types = cols(),
    col_names = c(
      "tag",
      "c1",
      "sex",
      "c2",
      "year",
      "c3",
      ages
    )
  ) %>%
    select(-c(1, 2, 4, 6)) %>%
    mutate(sex = recode(
      sex,
      MALES = "male",
      FEM = "female"
    )) %>%
    gather(key = "age", value = "count", -sex, -year) %>%
    mutate(age = as.integer(age))

  # get all the samples and put them in a long tibble
  sampfile <- tempfile()
  S[str_detect(S, "^GenotypesByAllelicType")] %>%
    cat(., sep = "\n", file = sampfile)

  samples <- read_delim(
    sampfile,
    col_types = cols(),  # silence the message...
    delim = ":",
    trim_ws = TRUE,
    col_names = FALSE
  ) %>%
    select(2,3) %>%
    rename(
      ID = X2,
      syears = X3
    ) %>%
    mutate(sampling_year = str_split(syears, "  *")) %>%
    unnest(sampling_year) %>%
    mutate(sampling_year = as.integer(sampling_year)) %>%
    select(-syears) %>%
    extract(
      ID,
      into = c("sex", "born_year"),
      regex = "^([MF])([0-9]+)_[0-9]+",
      remove = FALSE,
      convert = TRUE
      )

  # get the recapture pairs. This is a somehat complicated
  # thing that expands recaptures to all possible pairs of recaptures
  RC <- recapture_pairs(samples)
  PO <- parent_offspring_pairs(samples, pedigree)
  SIB <- sibling_pairs(samples, pedigree)

  # at some point I will want to add other relationships like
  # grandparental and avuncular...

  # return all the tibbles in a list
  list(
    pedigree = pedigree,
    census = census,
    samples = samples,
    pairs = bind_rows(
      RC,
      PO,
      SIB
    )
  )
}
