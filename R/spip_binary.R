######### spip funcs  ##########
#' return the path where spip should be in the R system paths
spip_binary_path <- function() {
  bin_name <- paste("spip", Sys.info()["sysname"], sep = "-")
  if(Sys.info()["sysname"] == "Windows") {
    bin_name <- paste(bin_name, ".exe", sep = "")
  }
  file.path(system.file(package = "CKMRpop"), "bin", bin_name)
}

#' return TRUE if ms exists where it should be
#'
spip_exists <- function() {
  file.exists(spip_binary_path())
}



#' file path to be used in a call to spip
#'
#' This version checks to make sure it is there and throws an
#' error with a suggestion of how to get it if it is not there.
#' @export
spip_binary <- function() {
  if(!spip_exists()) stop("Can't find the spip executable where it was expected
                        at ", spip_binary_path(), ".")

  # then I should check to make sure it is executable
  # though I have not implemented that...

  # if so, return the path
  spip_binary_path()

}

