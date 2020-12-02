
#' here is a function to plot a basic ancestry match matrix
#'
#' It is primarily for internal use
#' @param ATP a tibble that has ind_1, ind_2, and an ID column
#' @keywords internal
basic_amm_plot <- function(ATP, add_imps = FALSE) {
  g <- ggplot(ATP, aes(x = ind_1, y = ind_2, fill = amm)) +
    geom_tile(colour = "black") # put this down to establish a discrete scale
  g <- gg_add_generation_bands(
    g = g,
    L = max(ATP$x),
    add_impossibles = add_imps,
    alpha = 0.3
  ) +
    scale_fill_manual(values = c(`FALSE` = NA, Impossible = "white", `TRUE` = "black")) +
    geom_tile(colour = "black") +  # put it down again to have it on top
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5)
    )

  g
}

#' Count up the number of different kinds of related pairs and make a plot
#'
#' This counts up all the different types of pairwise relationships and
#' makes a plot (or multiple pages of them).
#' @param Pairs a tibble like that returned by `compile_related_pairs()`.
#' @param nrow the number of rows to plot per page
#' @param ncol the number of columns to plot per page
#' @export
count_and_plot_ancestry_matrices <- function(
  Pairs,
  nrow = 6,
  ncol = 5
) {


  #### First, we treat each distinct relationship type ####
  # count the number of different unique relationships
  anc_mat_counts <- Pairs %>%
    count(anc_match_matrix) %>%
    arrange(desc(n))

  # record how many relationship types there are
  n_relat <- nrow(anc_mat_counts)

  amc_to_plot <- anc_mat_counts %>%
    mutate(ID = str_c(sprintf("%03d", 1:n()), " (", n, ")" )) %>%
    mutate(amm_as_tib = map(anc_match_matrix, amm2tibble)) %>%
    unnest(amm_as_tib) %>%
    mutate(
      ind_1 = factor(ancestor_abbrvs(max(x))[x], levels = ancestor_abbrvs(max(x))),
      ind_2 = factor(ancestor_abbrvs(max(y))[y], levels = ancestor_abbrvs(max(y)))
    ) %>%
    mutate(amm = as.character(amm))


  # now we can lapply over the different pages
  num_pages <- ceiling(n_relat / (nrow * ncol))

  plots <- lapply(1:num_pages, function(i) {
    g <- basic_amm_plot(amc_to_plot) +
      ggforce::facet_wrap_paginate(~ ID, ncol = ncol, nrow = nrow, page = i)

    g
  })

  #### Now, we will also compile according to the dominant relationships ####
  # first tally up according to dominant relationship, dr_hits, and ancestry matrix together
  # and count up the total of each dom category while at it.  And also make an ID for each
  # of those ancestry match matrices that includes the dominant relationship, the dr_hits, and
  # how many were seen
  dr_counts  <- Pairs %>%
    count(dom_relat, dr_hits, max_hit, anc_match_matrix) %>%
    group_by(dom_relat) %>%
    mutate(tot_dom = sum(n)) %>%
    ungroup() %>%
    arrange(desc(tot_dom), desc(n)) %>%
    group_by(dom_relat) %>%
    mutate(
      ID = sprintf(
        "%03d-%s[%d,%d] - %d",
        1:n(),
        dom_relat,
        map_int(dr_hits, function(x) x[1]),
        map_int(dr_hits, function(x) x[2]),
        n
      )
    ) %>%
    ungroup()

  # now, figure dominant relationship has the most:
  max_panels <- dr_counts %>%
    count(dom_relat) %>%
    pull(n) %>%
    max()

  # now, choose a layout for that in which there might be one or a couple more rows than colums.
  drows <- ceiling(sqrt(max_panels))
  dcols <- ceiling(max_panels / drows)

  # now, set those IDs and dom_relats as factors so that
  # things plot out in the correct order, and then expand the
  # AMMs into rows in a tibble, then split all that
  # it into a list of tibbles on the dominant relationships
  dr_list <- dr_counts %>%
    group_by(anc_match_matrix) %>%
    mutate(
      ID = factor(ID, levels = unique(ID)),
      dom_relat = factor(dom_relat, levels = unique(dom_relat))
    ) %>%
    mutate(amm_as_tib = map(anc_match_matrix, amm2tibble)) %>%
    unnest(amm_as_tib) %>%
    mutate(
      ind_1 = factor(ancestor_abbrvs(max(x))[x], levels = ancestor_abbrvs(max(x))),
      ind_2 = factor(ancestor_abbrvs(max(y))[y], levels = ancestor_abbrvs(max(y)))
    ) %>%
    mutate(amm = as.character(amm)) %>%
    split(., .$dom_relat)

  # now we lapply over those and make a faceted ggplot for each
  dr_plots <- lapply(names(dr_list), function(n) {
    g <- basic_amm_plot(dr_list[[n]]) +
      ggtitle(paste0("Dominant relationship: ", n)) +
      ggforce::facet_wrap_paginate(~ ID, ncol = dcols, nrow = drows, page = 1)
  })
  names(dr_plots) <- names(dr_list)


  #### Finally, highly summarize the dr_counts into the dominant category and the dr_hit ####
  highly_summarised <- dr_counts %>%
    group_by(dom_relat, max_hit) %>%
    summarise(n = sum(n)) %>%
    arrange(desc(n)) %>%
    ungroup()

  #### now return a list ####
  list(
    highly_summarised = highly_summarised,
    dr_counts = dr_counts,
    dr_plots = dr_plots,
    anc_mat_counts = anc_mat_counts,
    anc_mat_plots = plots
  )

}
