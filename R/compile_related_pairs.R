#' compile pairwise relationships from the samples
#'
#' Run this on some of the output from `slurp_spip()`.
#' @param S a tibble. In the context of this package this tibble is
#' typically going to often be the
#' `samples` component of the output slurped up from spip with `slurp_spip()`.
#' More generally, it is a tibble that must have the columns:
#' - `ID`: the id of the sample
#' - `ancestors`: a list column of the ancestor vectors of each individual
#' - `relatives`: a list column of the vectors of individual samples (including self)
#' that each indvidual is related to.
#' @return a tibble with columns `id_1` and `id_2` for each pair.  Any additional
#' columns outside of `relatives` will be joined with `_1` and
#' `_2` suffixes.
#' @export
#' @examples
#' C <- compile_related_pairs(three_pops_with_mig_slurped_results$samples)
compile_related_pairs <- function(S) {

  # toss the sampled indivs with no relatives
  S2 <- S %>%
    filter(map_int(relatives, length) > 1)

  # keep just the columns that we will join in later
  S2_joiner <- S2 %>%
    select(-relatives)

  # find all pairs, in a lexicographically sorted order
  pairs_mat <- lapply(S2$relatives, function(x) {
    t(combn(sort(x), 2))
  }) %>%
    do.call(rbind, .)
  colnames(pairs_mat) <- c("id_1", "id_2")

  pairs_tib_1 <- as_tibble(pairs_mat)

  # count how many times each pair occurs. I think that all those that
  # occur only once will not end up being relatives. They just happen to both
  # be related someone else, but they are not closely enough related to count.
  # So, I will just record that now and verify.  Ultimately, we can probably toss them
  S2_1 <- S2_joiner
  names(S2_1) <- paste0(names(S2_1), "_1")
  S2_2 <- S2_joiner
  names(S2_2) <- paste0(names(S2_2), "_2")

  # join those together and compute the ancestor-matching matrix (the anc_match_matrix).
  # and, at the end, determine the "primary shared ancestors" (see the paper for an explanation).
  pairs_tib_2 <- pairs_tib_1 %>%
    count(id_1, id_2) %>%
    filter(n > 1) %>%  # toss out the ones only seen once because they are not true relationships
    left_join(S2_1, by = c("id_1" = "ID_1")) %>%
    left_join(S2_2, by = c("id_2" = "ID_2")) %>%
    mutate(
      anc_match_matrix = map2(
        .x = ancestors_1,
        .y = ancestors_2,
        .f = function(x, y) outer(x, y, "==")
      )
    )  %>%
    mutate(no_relat = map_lgl(.x = anc_match_matrix, .f = function(x) all(x == FALSE))) %>%
    filter(no_relat == FALSE) %>%
    select(-no_relat) %>%
    mutate(
      primary_shared_ancestors = map(.x = anc_match_matrix, .f = primary_ancestor_pairs)
    )  %>%
    mutate(psa_tibs = map(  # also save those primary ancestors in tibble form
      .x = primary_shared_ancestors,
      .f = function(m) {
        tibble(
          prim_anc_1 = map_int(m, 1),
          prim_anc_2 = map_int(m, 2)
        )
      }
    ))

    # now, we want categorize the "dominant relationship" of each pair.  This is done by
    # cycling over (within a function) relationships from Self to PO to Sib to Aunt, etc
    # and returning the first one that qualifies.  We also return how many hits are within
    # each of those relationships zones. Then we pull the list elements type and hits into
    # their own columns
    pairs_tib_3 <- pairs_tib_2 %>%
      mutate(
        dr = map(
          .x = anc_match_matrix,
          .f = function(x) {cat_dom_relat(x)}
        ),
        dom_relat = map_chr(dr, function(x) x$type),
        dr_hits = map(dr, function(x) x$hits)
      ) %>%
      select(-dr) %>%
      mutate(
        upper_member = map2_int(
          .x = dom_relat,
          .y = dr_hits,
          .f = function(x, y) {
            if(x %in% c("Se", "Si", "FC"))
              return(NA_integer_)
            else if(y[1] == y[2])
              return(0L)
            else if(y[1] > y[2])
              return(1L)
            else if(y[1] < y[2])
              return(2L)
            else
              return(-999L)  # flags that things got through here and should not have
          }
        ),
        max_hit = map_int(
          .x = dr_hits,
          .f = function(x) max(x)
        )
      )

    # we will return that for now, after adding conn_comps to it
    pairs_tib_3 %>%
      rename(times_encountered = n) %>%
      select(
        id_1,
        id_2,
        dom_relat,
        max_hit,
        dr_hits,
        upper_member,
        times_encountered,
        primary_shared_ancestors,
        psa_tibs,
        starts_with("pop_"),
        starts_with("sex_"),
        starts_with("born_year_"),
        starts_with("samp_years_list"),
        everything()
      ) %>%
      relpair_conn_comps()

}
