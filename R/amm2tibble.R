#' convert an ancestry-matching matrix to a ggplot-able tibble
#'
#' This merely converts a matrix to a tibble that can be plotted
#' easily using ggplot.
#' @param M an ancestry-match matrix
#' @export
#' @examples
#' # convert one of the simple example AMMs to a tibble
#' amm2tibble(example_amms$Father_Offspring_2gen)

amm2tibble <- function(M) {
  tibble(
    x = rep(1:nrow(M), ncol(M)),
    y = rep(1:ncol(M), each = nrow(M)),
    amm = as.vector(M)
  )
}
