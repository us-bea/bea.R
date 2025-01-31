.onLoad <- function(lib, pkg) {
  packageStartupMessage("Note 1: As of February 2018, beaGet() requires 'TableName' for NIPA and NIUnderlyingDetail data instead of 'TableID.' See https://github.us-bea/bea.R for details.")
  packageStartupMessage("Note 2: The BEA API no longer releases regional data under the RegionalData, RegionalIncome, or RegionalProduct dataset names; please use 'DatasetName'='Regional' for regional data instead.")
}
