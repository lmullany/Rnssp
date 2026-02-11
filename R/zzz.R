.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the
 ____
|  _ \\ _ __  ___ ___ _ __
| |_) | '_ \\/ __/ __| '_ \\
|  _ <| | | \\__ \\__ \\ |_) |
|_| \\_\\_| |_|___/___/ .__/   package version 0.3.1,
                    |_|
A Signature R package for the National Syndromic Surveillance Program at
the Centers for Disease Control and Prevention (CDC)

Full Documentation available at: https://cdcgov.github.io/Rnssp
Rnssp RMD Templates Documentation: https://cdcgov.github.io/Rnssp-rmd-templates

Run 'Rnssp_vignettes()' to browse all Rnssp vignettes.
")
  
  # if we are running R CMD check, we shouldn't do this network call:
  if(!interactive() || nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
    return(invisible())
  }
  
  # Otherwise, we can check, although this should really be a user option
  
  url <- "https://raw.githubusercontent.com/cdcgov/Rnssp/master/DESCRIPTION"
  
  x <- try(readLines(url, warn = FALSE), silent = TRUE)
  if (inherits(x, "try-error")) return(invisible())
  
  ver_line <- x[grep("^Version:", x)]
  if (length(ver_line) == 0) return(invisible())
  
  remote_version_chr <- sub("^Version:\\s*", "", ver_line[1])
  remote_version <- try(numeric_version(remote_version_chr), silent = TRUE)
  if (inherits(remote_version, "try-error")) return(invisible())
  
  installed_version <- utils::packageVersion("Rnssp")
  
  if (installed_version < remote_version) {
    cli::cli_alert_info(
      paste0(
        "Rnssp v", installed_version,
        " is outdated!\n## Please upgrade to latest Rnssp v",
        remote_version,
        ': {.code devtools::install_github("cdcgov/Rnssp")}'
      )
    )
  }
  
  invisible()
}

