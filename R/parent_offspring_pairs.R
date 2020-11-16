#' Return parent-offspring pairs amongst the samples
#'
#' Given the true pedigree and a list of sampled individuals
#' return a tibble of all the parent offspring pairs.  Note that
#' if the individuals
#' were sampled multiple times, each of those different pairs
#' will show up.  This allows the covariate information to be used
#' in the model as well.
#'
#' In this case the parent is always going to be ID_1 and the offspring
#' will be ID_2, in the output.
#' The column pair_type in the output takes the value "PO" for parent-offspring.
#' @param S the samples tibble with columns ID, sex, born_year, sampling_year
#' @param P the pedigree with columns year, kid, pa, ma
#' @export
parent_offspring_pairs <- function(S, P) {
  # first the father-offspring pairs:
  dads <- left_join(S, P, by = c("ID" = "kid")) %>%
    select(-year, -ma) %>%
    rename(ID_2 = ID) %>%
    inner_join(
      S,
      by = c("pa" = "ID"),
      suffix = c("_2", "_1")
    ) %>%
    rename(
      ID_1 = pa
    ) %>%
    mutate(pair_type = "PO") %>%
    select(pair_type, ends_with("_1"), ends_with("_2"))

  # then get the mother offspring pairs
  moms <- left_join(S, P, by = c("ID" = "kid")) %>%
    select(-year, -pa) %>%
    rename(ID_2 = ID) %>%
    inner_join(
      S,
      by = c("ma" = "ID"),
      suffix = c("_2", "_1")
    ) %>%
    rename(
      ID_1 = ma
    ) %>%
    mutate(pair_type = "PO") %>%
    select(pair_type, ends_with("_1"), ends_with("_2"))


  bind_rows(dads, moms) %>%
    arrange(sampling_year_1)
}
