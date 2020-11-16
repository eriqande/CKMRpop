#' compile pairwise relationships from the samples
#'
#' Run this on some of the output from `slurp_spip()`.
#' @param S a tibble that must have the columns:
#' - `ID`: the id of the sample
#' - `ancestors`: a list column of the ancestor vectors of each individual
#' - `relatives`: a list column of the vectors of individual samples (including self)
#' that each indvidual is related to.
#' @return a tibble with columns `id_1` and `id_2` for each pair.  Any additional
#' columns outside of `relatives` will be joined with `_1` and
#' `_2` suffixes. There are other columns in the return too.... More later.
#' @export
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

  # join those together and compute the ancestor-matching matrix (the anc_match_matrix)
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
    select(-no_relat)

    # now, we want to make a bit-string to categorize each type of relationship,
    # so that we can figure out how many different relationship types there are.
    pairs_tib_3 <- pairs_tib_2 %>%
      mutate(
        bit_string = map_chr(
          .x = anc_match_matrix,
          .f = function(x) {paste(as.integer(as.vector(x)), collapse = "")}
        )
      )

    # we will return that for now


}
