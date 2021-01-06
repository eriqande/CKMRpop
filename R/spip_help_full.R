
#' print the full usage information from spip
#'
#' This simply calls spip with the `--help-full` option.
#' @export
#' @examples
#' \dontrun{spip_help()}
spip_help_full <- function() {
  system2(command = spip_binary(), args = "--help-full")
}
