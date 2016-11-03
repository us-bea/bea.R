#' Convert BEA API httr response payload to list
#' 
#' @param beaPayload An object with httr class 'response' from call to BEA API
#' @param isMeta 	 Special parameter meant to interact with metadata functions (default: FALSE)
#' @return An object of class 'list' of several dimensions. View list structure using 'str(yourList)'.
#' @import httr
#' @importFrom jsonlite fromJSON
#' @export
#' @examples 
#' userSpecList <- list('UserID' = 'yourKey' ,
#'									'Method' = 'GetData',
#'									'datasetname' = 'NIPA',
#'									'Frequency' = 'A',
#'									'TableID' = '68',
#'									'Year' = 'X')
#' resp <- beaGet(userSpecList, asTable = FALSE)
#' BL <- bea2List(resp)


bea2List <- function(beaPayload, isMeta=FALSE) {
	requireNamespace('httr', quietly = TRUE)
	requireNamespace('jsonlite', quietly = TRUE)
	if(class(beaPayload) != 'response'){
		warning('Submitted variable is not a valid httr response class object.')
		return('Submitted variable is not a valid httr response class object.')
	}
	
	if(floor(beaPayload$status_code/100) != 2){
		stop(
			paste0('Request failed. Returned HTTP status code: ', beaPayload$status_code),
			call. = FALSE
		)
	}
	
	
#Fix later: I gave up on parsing it identically and just re-pull data as JSON
		if(length(grep("resultformat=xml", beaPayload$url, ignore.case=TRUE))==1){
			beaJSON <- httr::GET(
				gsub(
					"resultformat=xml", 
					"ResultFormat=json", 
					beaPayload$url, 
					ignore.case=TRUE
				)
			)
			
			beaContent <- httr::content(beaJSON, as = 'text')
		}
		else {
			beaContent <- httr::content(beaPayload, as = 'text')
		}

		beaResponse <- jsonlite::fromJSON(beaContent)

		if('error' %in% tolower(
			attributes(
				beaResponse$BEAAPI$Results
			)$names)
		){
			warning(beaResponse$BEAAPI$Results$Error$APIErrorDescription)
			return(beaResponse$BEAAPI$Results)
		}
		
		if(isMeta){
			beaList <- beaResponse$BEAAPI$Results
			attributes(beaList)$params <- beaResponse$BEAAPI$Request$RequestParam
		} else {
			beaList <- beaResponse$BEAAPI$Results$Data
			attributes(beaList)$params <- beaResponse$BEAAPI$Request$RequestParam
			attributes(beaList)$detail <- beaResponse$BEAAPI$Results[(
				attributes(beaResponse$BEAAPI$Results)$names != 'Data'
			)]
			beaList$DataValue <- as.numeric(
				gsub(',', '',  beaList$DataValue, fixed = TRUE)
			)
		}
		#Use jsonlite fromJSON f(x) to convert to list 
		return(beaList)
}
