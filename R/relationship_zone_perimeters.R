
#' Return the perimeters of all the relationship zones
#'
#' This is primarily for plotting a figure in the paper about this package,
#' showing where all the relationship zones are.  It merely cycles over the
#' possible relationships in [`relationship_zone_names`] and produces one or
#' two rows in a tibble for each that has the corners of the rectangle of that
#' zone in the columns xmin, xmax, ymin, and ymax.  It is designed to be overlaid
#' upon the ancestry_match_matrix plots. There are some additional columns give
#' us the midpoint of the area, etc.
#' @export
relationship_zone_perimeters <- function() {
  lapply(relationship_zone_names, function(R) {
    tmp_tib <- lapply(anc_match_masks(4, R), function(m) {
      xmin <- min(which(apply(m, 2, function(x) any(x==TRUE))))
      xmax <- max(which(apply(m, 2, function(x) any(x==TRUE))))
      ymin <- min(which(apply(m, 1, function(x) any(x==TRUE))))
      ymax <- max(which(apply(m, 1, function(x) any(x==TRUE))))

      tibble(
        zone = R,
        xmin = xmin - 0.5,
        xmax = xmax + 0.5,
        ymin = ymin - 0.5,
        ymax = ymax + 0.5
      )
    }) %>%
      bind_rows(.id = "which_matrix")

    if(R %in% c("Se", "Si", "FC", "SC", "TC")) {
      tmp_tib <- tmp_tib[1,]
    }
    tmp_tib
  }) %>%
    bind_rows() %>%
    mutate(
      which_matrix = paste0("M", which_matrix)
    )

}
