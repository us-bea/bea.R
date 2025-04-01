.onLoad <- function(libname, pkgname) {
  # Optionally bypass startup code when R CMD check is running
  if (nzchar(Sys.getenv("RCMDCHECK", ""))) return()
  
  if (system.file("help", package = pkgname) == "") return()
  
  ns <- .getNamespace(pkgname)
  if (is.null(ns)) stop("cannot find namespace environment for ", pkgname, domain = NA)

  dbbase <- file.path(libname, pkgname, "R", pkgname)
  lazyLoad(dbbase, ns, filter = function(n) n != ".__NAMESPACE__.")
}
.onAttach <- function(libname, pkgname) {
  # startup message
  if (interactive()) {
    packageStartupMessage("Note 1: As of February 2018, beaGet() requires 'TableName' for NIPA and NIUnderlyingDetail data instead of 'TableID.' See https://github.us-bea/bea.R for details.")
    packageStartupMessage("Note 2: The BEA API no longer releases regional data under the RegionalData, RegionalIncome, or RegionalProduct dataset names; please use 'DatasetName'='Regional' for regional data instead.")
    invisible()
  }
}
