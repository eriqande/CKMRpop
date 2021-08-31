

#' Find relatives amongst samples from their ancestor vectors
#'
#' Given the ancestry vectors of a collection of individuals
#' (like the set of all sampled individuals from a simulation)
#' this function just figures out who is a relative of whom.
#' Relatives are defined as individuals that share at least
#' one ID within their ancestry vectors.  The way this function
#' works is by, for each ID, making a vector of all the other IDs
#' that include that original ID in its ancestry vectors.  Then,
#' for each individual, these vectors are catenated (one for each
#' ancestor) then values are made unique and sorted, and returned in
#' a list column of relatives.
#' @param A a tibble that has columns of `ID` and `ancestors`. `ID` is the ID of the
#' individual and `ancestors` is a list column that holds ancestry vectors.
#' @return This function returns the original input, `A`, with an additional
#' list column called `relatives`, that includes vectors of the samples that
#' each sample is related to.
#' @export
relatives_from_ancestry_vectors <- function(A) {
  tmp <- A %>%
    unnest(ancestors) %>%
    group_by(ancestors) %>%
    summarise(descendants = list(unique(ID)))

  desc <- tmp$descendants
  names(desc) <- tmp$ancestors

  A %>%
    mutate(
      relatives = map(
        .x = ancestors,
        function(x) sort(unique(unlist(desc[as.character(x)])))
      )
    )

}
