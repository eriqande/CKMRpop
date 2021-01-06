

#' Download the spip binary and install it where CKMRpop expects it
#'
#' This checks the operating system and installs the correct version
#' (either Darwin or Linux for Mac or Linux, respectively.)
#' @param Dir the directory to install spip into.  Because of restrictions
#' on functions writing to the user's home filespace, this is set, by default,
#' to a temporary directory.  But to really use this function to install spip,
#' this parameter must be set to `system.file(package = "CKMRpop")`.
#' @export
install_spip <- function(
  Dir = tempfile()
) {

  if(Dir != system.file(package = "CKMRpop")) {
    message("\n*** Note: To properly install spip, the function install_spip() must be called like this: ***\n\n    install_spip(Dir = system.file(package = \"CKMRpop\"))
\n*** The current invocation of install_spip() will not properly install it. ***")
  }

  # first check the OS
  Sys <- Sys.info()["sysname"]

  if(!(Sys %in% c("Darwin", "Linux"))) {
    stop(paste("spip binary not available for operating system ", Sys, collapse = ""))
  }

  # then get the basename of the file we want
  pname <- paste("spip-", Sys, sep = "", collapse = "")

  # have a variable to hold the base GitHub address:
  Git_base <- "https://github.com/eriqande/spip/raw/master/"

  Git_full <- paste(Git_base, pname, sep = "")

  # get the destination path and create the directory
  Dest_dir <- file.path(Dir, "bin")
  dir.create(Dest_dir, showWarnings = FALSE, recursive = TRUE)

  # record destination file name
  Dest_file <- file.path(Dest_dir, pname)

  # now, download the file and save to the correct destination:
  utils::download.file(url = Git_full, destfile = Dest_file)

  # finally, change the file permissions to be user and group executable and writeable
  # and world readable
  Sys.chmod(Dest_file, mode = "0774", use_umask = FALSE)
}
