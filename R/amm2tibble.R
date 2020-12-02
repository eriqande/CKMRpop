#' convert an ancestry-matching matrix to a ggplot-able tibble
#'
#' @param M an ancestry-match matrix
#' @export
amm2tibble <- function(M) {
  tibble(
    x = rep(1:nrow(M), ncol(M)),
    y = rep(1:ncol(M), each = nrow(M)),
    amm = as.vector(M)
  )
}
