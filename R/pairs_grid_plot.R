#' plot all the related pairs in a facet grid
#'
#' blah blah
#' @param pairs the pairs tibble from slurp_spip()
#' @param x_buffer fraction of the unit interval on the left and right that
#' things don't get jittered into.
#' @param y_buffer fraction of the unit interval on the top and bottom that
#' things don't get jittered into.
#' @return sends back a ggplot object
#' @export
pairs_grid_plot <- function(
  pairs,
  x_buffer = 0.05,
  y_buffer = 0.05
) {
  # add uniform random jigger to them to keep them near their
  # true age and
  p2 <- pairs %>%
    mutate(
      xjit = runif(n(), x_buffer, 1 - x_buffer) - 0.5,
      yjit = runif(n(), y_buffer, 1 - y_buffer) - 0.5,
      nojit_age1 = sampling_year_1 - born_year_1,
      nojit_age2 = sampling_year_2 - born_year_2,
      age_1 =  nojit_age1 + yjit,
      age_2 =  nojit_age2 + yjit,
      sy_1 = sampling_year_1 + xjit,
      sy_2 = sampling_year_2 + xjit,
      pair_sexes = paste0(sex_1, "-", sex_2)
    )

  ggplot(p2) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    geom_vline(
      xintercept = (min(p2$sampling_year_1) - 0.5):(max(p2$sampling_year_2) + 0.5),
      colour = "gray",
      size = 0.3
    ) +
    geom_hline(
      yintercept = (min(c(p2$nojit_age1, p2$nojit_age2)) - 0.5):(max(c(p2$nojit_age1, p2$nojit_age2)) + 0.5),
      colour = "gray",
      size = 0.3
    ) +
    geom_segment(
      aes(
        x = sy_1,
        xend = sy_2,
        y = age_1,
        yend = age_2,
      ),
      lineend = "round"
    ) +
    facet_grid(pair_type ~ pair_sexes) +
    xlab("Year of sampling") +
    ylab("Age at sampling")
}
