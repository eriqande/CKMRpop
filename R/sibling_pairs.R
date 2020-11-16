# First, a simple function to make unique pairs from indivs sampled on the same day
take_one <- function(D) {
  D %>%
    filter(sampling_year_1 == sampling_year_2) %>%
    mutate(tag = paste(pmin(ID_1, ID_2), pmax(ID_1, ID_2), sep = "--")) %>%
    arrange(ID_1, ID_2) %>%
    group_by(tag) %>%
    slice(1) %>%
    ungroup() %>%
    select(-tag)
}

#' Return the sibling pairs from amongst the samples
#'
#' Given the true pedigree and a list of sampled individuals
#' return a tibble of all the full_sibling and half-sibling pairs.  Note that
#' if the individuals
#' were sampled multiple times, each of those different pairs
#' will show up.  This allows the covariate information to be used
#' in the model as well.
#'
#' The column pair_type shows FS for full siblings and MHS for maternal half-siblings
#' and PHS for paternal half siblings.  Those two categories cannot always
#' be resolved in practice (though with mtDNA sequences it might be possible...)
#' @param S the samples tibble with columns ID, sex, born_year, sampling_year
#' @param P the pedigree with columns year, kid, pa, ma
#' @export
sibling_pairs <- function(S, P) {

  #### first, get the full sibling pairs ####
  Sp <- left_join(S, P, by = c("ID" = "kid")) %>%
    filter(pa != "0" & ma != "0") %>%
    select(-year) %>%
    rename_all(.funs = function(x) paste0(x, "_1"))
  Sp2 <- Sp %>%
    rename_all(.funs = function(x) str_replace(x, "_1$", "_2"))
  tmp <- inner_join(Sp, Sp2, by = c("pa_1" = "pa_2", "ma_1" = "ma_2")) %>%
    filter(ID_1 != ID_2) %>%
    select(-pa_1, -ma_1)

  # all the pairs that were sampled in different years
  dsy <- tmp %>%
    filter(sampling_year_1 < sampling_year_2)
  # we have to do something special for pairs sampled in the same year.
  # We make an ID for either direction we look at it, and then keep just the first
  ssy <- take_one(tmp)

  full_sibs <- bind_rows(
    dsy,
    ssy
  ) %>%
    mutate(pair_type = "FS")

  #### Then, in a similar fashion, we get the MHS and the PHS pairs ####

  # maternal half sibs
  m_tmp <- inner_join(Sp, Sp2, by = c("ma_1" = "ma_2")) %>%
    filter(ID_1 != ID_2) %>%
    select(-pa_1, -ma_1, -pa_2)
  mdsy <- m_tmp %>%
    filter(sampling_year_1 < sampling_year_2)
  mssy <- take_one(m_tmp)
  mhs_pre_toss <- bind_rows(
    mdsy,
    mssy
  ) %>%
    mutate(pair_type = "MHS")

  # paternal half sibs
  p_tmp <- inner_join(Sp, Sp2, by = c("pa_1" = "pa_2")) %>%
    filter(ID_1 != ID_2) %>%
    select(-pa_1, -ma_1, -ma_2)
  pdsy <- p_tmp %>%
    filter(sampling_year_1 < sampling_year_2)
  pssy <- take_one(p_tmp)
  phs_pre_toss <- bind_rows(
    pdsy,
    pssy
  ) %>%
    mutate(pair_type = "PHS")

  # now, strip the full sibs out of the half sibs data frame
  # and bung everyone together
  bind_rows(
    mhs_pre_toss,
    phs_pre_toss
  ) %>%
    anti_join(., full_sibs, by = c("ID_1", "ID_2")) %>%
    bind_rows(full_sibs) %>%
    select(pair_type, everything()) %>%
    arrange(pair_type, sampling_year_1, sampling_year_2, ID_1, ID_2)
}
