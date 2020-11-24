#' Run spip in a user-specified directory
#'
#' This runs it in a directory and the output from stdout
#' goes into a big file spip_out.txt in that directory. Currently
#' this is pretty bare bones.
#' @param pars A named list of parameter values.
#' @param dir The directory to run it in.  Defaults to a temp directory,
#' which will be unique every time it is run.
#' @param spip_seeds a vector of two positive integers. These get written to the
#' file spip_seeds, which is used by spip to seed its random number generator.  By
#' default, R supplies these two integers from its own random number generator.  This
#' way reproducible results from spip can be obtained by calling `set.seed()` from within
#' R before calling `run_spip()`.  For the most part, the user should never really have
#' to directly supply a value for spip_seeds.
#' @export
run_spip <- function(
  pars,
  dir = tempfile(),
  spip_seeds = ceiling(runif(2, 1, 1e9))
) {

  cwd = getwd()  # get the current directory to change back to it after the system2 call


  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  # write parameters to demog.comm
  dfile <- file.path(dir, "demog.comm")
  sfile <- file.path(dir, "spip_out.txt")
  efile <- file.path(dir, "spip_err.txt")
  lfile <- file.path(dir, "alle_freq.txt")
  seedfile <- file.path(dir, "spip_seeds")

  # write the spip_seeds file in dir
  cat(spip_seeds, sep = " ", eol = "\n", file = seedfile)

  cat("1\n2 0.5 0.5\n", file = lfile)

  cat("&  generated from R &\n", file = dfile)
  dump <- lapply(names(pars), function(n) {
    # R is inconsistent in type of dash it prints, so standardize with
    # this line:
    n_clean <- gsub("\\p{Pd}", "-", n, perl = TRUE)
    cat(
      "--", n_clean, " ", paste(pars[[n]], collapse = " "), "\n",
      sep = "",
      file = dfile,
      append = TRUE
    )
  })

  args <- paste(" --num-pops 1 --command-file ", dfile, " --locus-file ", lfile )


  message("Running spip in directory ", dir)

  # Run this in system2
  setwd(dir)
  system2(
    command = spip_binary(),
    args = args,
    stdout = sfile,
    stderr = efile
  )

  message(
    "Done running spip. Output file size is ",
    file.size(sfile) / 1e6,
    " Mb"
  )

  message("Processing output file with awk")

  # now, use awk to process that large text file into some things that
  # can be read in by R.  For very large output files, I think that
  # awk will almost certainly be faster.
  single_pass <- system.file("shell/single_pass_cps.sh", package = "CKMRpop")
  system2(
    command = single_pass,
    stdout = "single_pass_stdout.txt",
    stderr = "single_pass_stderr.txt"
  )

  message("Done processing output into spip_pedigree.tsv, spip_prekill_census.tsv, and spip_samples.tsv")

  # change back to the original working directory
  setwd(cwd)

  # here is how I used to call it using system()
  # call <- paste("./bin/spip --command-file ", dfile, " --locus-file ", lfile, " > ", sfile )
  # system(call)

  # return the path of the directory where everything happened
  dir
}
