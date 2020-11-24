
#' Just a simple plot function
#'
#' Easy to do, but quicker to have it wrapped up in a plot.
#' @param census a tibble of census counts with columns `year` and
#' `age`, and then the counts of the different sexes in columns
#' named `male`, and `female`.
#' @return a ggplot object
#' @export
ggplot_census_by_year_age_sex <- function(census) {
  g <- census %>%
    pivot_longer(
      cols = c(male, female),
      names_to = "sex",
      values_to = "n"
    ) %>%
    ggplot(
      aes(
        x = year,
        y = n,
        fill = as.factor(age)
      )
    ) +
    geom_col(colour = "black", size = 0.1) +
    facet_grid(pop ~ sex) +
    guides(
      fill = guide_legend(title = "Age")
    )

  g
}
