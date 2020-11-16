
#' Return a vector of ancestor IDs for each individual
#'
#' The goal here ultimately is to reduce a large pedigree to only the individuals
#' that we need, by dint of them being fairly close in the pedigree to the a group
#' of individuals that were sampled.  For each member of indivs, this function returns
#' a vector of its ancestors going back `num_generations` generations.  The ordering
#' is simple: 1 is the indiv, 2 is pa, 3 is ma, then 4 is pa's pa, 5 is pa's ma, 6 is
#' ma's pa, and 7 is ma's ma, then 8 is pa's pa's pa, and so forth, back as many
#' generations as one needs to go.
#' @param indivs a vector of IDs of individuals whose ancestors you want listed
#' @param ped a pedigree that includes all the individuals in `indivs` and also
#' their ancestors. It should be a tibble with columns `kid`, `pa` `ma`.
#' @param num_generations how many generations do you wish to record for
#' each sampled individual.  1 means just the individual themselves (so, not very
#' interesting, and you likely wouldn't ever use it, but it turns out to be handy,
#' for the programming, to define that level as 1); 2 means up to
#' and including the parents; 3 means up to and including the grandparents; 4 means up to
#' and including the great grandparents; and so forth.
#' @export
ancestor_vectors <- function(
  indivs,
  ped,
  num_generations
) {

  # add a row to ped to add kid "0" in there so that unknown founders
  # propagate backwards
  ped2 <- bind_rows(
    tibble(kid = "0", pa = "0", ma = "0"),
    ped
  )
  # first, turn the pedigree into a list
  plist <- lapply(1:nrow(ped2), function(i) {c(ped2$pa[i], ped2$ma[i])})
  names(plist) <- ped2$kid


  # then cycle over the indivs, and within that, cycle over the generations
  alist <- lapply(indivs, function(x) {
    ret <-  list()
    ret[[1]] <- x
    for(i in 2:num_generations) {
      ret[[i]] <- lapply(ret[[i - 1]], function(z) plist[[z]]) %>%
        unlist()
    }
    ret <- unlist(ret)
    ret[ret == "0"] <- NA  # for now, mark founders and before as NA...
    ret
  })

  alist
}
