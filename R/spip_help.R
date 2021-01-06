
#' print the abbreviated usage information from spip
#'
#' This simply calls spip with the `--help` option.
#' @export
#' @examples
#' \dontrun{spip_help()}
spip_help <- function() {
  system2(command = spip_binary(), args = "--help")
}
