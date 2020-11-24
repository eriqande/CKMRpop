
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
    g <- ggplot(amc_to_plot, aes(x = ind_1, y = ind_2, fill = amm)) +
      geom_tile(colour = "black") # put this down to establish a discrete scale
    g <- gg_add_generation_bands(
      g = g,
      L = max(amc_to_plot$x),
      add_impossibles = FALSE,
      alpha = 0.3
    ) +
      ggforce::facet_wrap_paginate(~ ID, ncol = ncol, nrow = nrow, page = i) +
      scale_fill_manual(values = c(`FALSE` = NA, Impossible = "white", `TRUE` = "black")) +
      geom_tile(colour = "black") +  # put it down again to have it on top
      theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5)
      )

      g
  })

  # now return a list
  list(
    anc_mat_counts = anc_mat_counts,
    plots = plots
  )

}
