#' Download BEA metadata into library/data folder if needed
#' 
#' @param datasetList list of BEA datasets to update local metadata file for (e.g., list('NIPA', 'FixedAssets'))
#' @param beaKey Your API key
#' @keywords metadata search
#' @return Nothing. This updates local .RData files to be used in beaSearch.
#' @import httr data.table
#' @importFrom jsonlite fromJSON
#' @export
#' @examples 
#' beaUpdateMetadata(list('RegionalData', 'NIPA'), beaKey = 'yourAPIkey')


beaUpdateMetadata <- function(datasetList, beaKey){

 'Datasetname'			<- NULL
 'MetaDataUpdated'	<- NULL
 'DatasetName'			<- NULL
 'TableID'					<- NULL
 'Line'							<- NULL
 '.'								<- NULL
 'SeriesCode'				<- NULL
 'RowNumber'				<- NULL
 'LineDescription'	<- NULL
 'LineNumber'				<- NULL
 'ParentLineNumber'	<- NULL
 'Tier'							<- NULL
 'Path'							<- NULL
 'APITable'					<- NULL
 'TableName'				<- NULL
 'ReleaseDate'			<- NULL
 'NextReleaseDate'	<- NULL
 'Parameter'				<- NULL
 'ParamValue'				<- NULL

	#datasetList <- list('nipa','niunderlyingdetail','fixedassets','regionaldata','regionalproduct','regionalincome')
	
	requireNamespace('data.table', quietly = TRUE)
	requireNamespace('httr', quietly = TRUE)
	requireNamespace('jsonlite', quietly = TRUE)

	beaMetadataStore <- paste0(.libPaths()[1], '/beaR/data')

	beaMetaSpecs <- list(
		'UserID' = beaKey ,
		'method' = 'GetData',
		'datasetname' = 'APIDatasetMetaData',
		'dataset' = paste(datasetList, collapse = ','),
		'ResultFormat' = 'json'
	)
	
	#Get as httr response
	beaResponse <- beaR::beaGet(beaMetaSpecs, asList = FALSE, asTable = FALSE, isMeta = TRUE)	
	
	#Check to ensure it is httr response
	if(class(beaResponse) != 'response'){
		warning('API metadata not returned.  Verify that you are using a valid API key, represented as a character string.')
		return('API metadata not returned.  Verify that you are using a valid API key, represented as a character string.')
	}
	
	lapply(datasetList, function(outdat){
		try(file.remove(paste0(beaMetadataStore,'/', outdat, '.RData')), silent = TRUE)
	})

	
	
	#Get JSON String
	respStr <- httr::content(beaResponse, as = 'text')
	
	#Actually, we can get this same info faster using GetParamValsList or something
	#The line below should be suppressed if fixed - JSON was malformed due to missing commas
	#respStr <- gsub('}{', '},{', respStr, fixed = TRUE)
	metaList <-jsonlite::fromJSON(respStr)
	metasetInfo <- as.data.table(metaList$BEAAPI$Datasets)
	if(dim(metasetInfo)[1] == 0){
		warning('API metadata not returned.  Verify that you are using a valid API key, represented as a character string.')
		return('API metadata not returned.  Verify that you are using a valid API key, represented as a character string.')
	}

	#bind dataset metadata together
	 #This is a bit of a time drag, so we want to only do it if we need to
	 #And do it separately for each dataset
	if('nipa' %in% tolower(datasetList)){try({
		nipaMDU <- metasetInfo[tolower(Datasetname) == 'nipa', MetaDataUpdated]
		nipaTabs <- rbindlist(metasetInfo[tolower(Datasetname) == 'nipa', APITable])
		nipaTabs[, DatasetName := 'NIPA']
		
		nipaRows <- rbindlist(lapply(nipaTabs[, TableID], function(thisTab){
			tabPart <- nipaTabs[TableID == thisTab, as.data.table(Line[[1]])]
			tabPart[, TableID := thisTab]
			return(tabPart)
		}))
		
		data.table::setkey(nipaTabs, key = TableID)
		data.table::setkey(nipaRows, key = TableID)
		
		nipaIndex <- nipaTabs[nipaRows][,.(
			SeriesCode, 
			RowNumber, 
			LineDescription, 
			LineNumber, 
			ParentLineNumber, 
			Tier, 
			Path, 
			TableID,
			DatasetName,
			TableName, 
			ReleaseDate,
			NextReleaseDate,
			MetaDataUpdated = nipaMDU
		)]
		save(nipaIndex, file=paste0(beaMetadataStore, '/NIPA.RData'))
	})}
	

	if('niunderlyingdetail' %in% tolower(datasetList)){try({
		niudMDU <- metasetInfo[tolower(Datasetname) == 'niunderlyingdetail', MetaDataUpdated]
		niudTabs <- rbindlist(metasetInfo[tolower(Datasetname) == 'niunderlyingdetail', APITable])
		niudTabs[, DatasetName := 'NIUnderlyingDetail']
		
		niudRows <- rbindlist(lapply(niudTabs[, TableID], function(thisTab){
			tabPart <- niudTabs[TableID == thisTab, as.data.table(Line[[1]])]
			tabPart[, TableID := thisTab]
			return(tabPart)
		}))
	 
		data.table::setkey(niudTabs, key = TableID)
		data.table::setkey(niudRows, key = TableID)
		
		niudIndex <- niudTabs[niudRows][,.(
			SeriesCode, 
			RowNumber, 
			LineDescription, 
			LineNumber, 
			ParentLineNumber, 
			Tier, 
			Path, 
			TableID,
			DatasetName,
			TableName, 
			ReleaseDate,
			NextReleaseDate, 
			MetaDataUpdated = niudMDU
		)]
	
		save(niudIndex, file=paste0(beaMetadataStore, '/NIUnderlyingDetail.RData'))
	})}
	
	
	if('fixedassets' %in% tolower(datasetList)){try({
		fixaMDU <- metasetInfo[tolower(Datasetname) == 'fixedassets', MetaDataUpdated]
		fixaTabs <- rbindlist(metasetInfo[tolower(Datasetname) == 'fixedassets', APITable])
		fixaTabs[, DatasetName := 'FixedAssets']
		
		fixaRows <- rbindlist(lapply(fixaTabs[, TableID], function(thisTab){
			tabPart <- fixaTabs[TableID == thisTab, as.data.table(Line[[1]])]
			tabPart[, TableID := thisTab]
			return(tabPart)
		}))
	
		data.table::setkey(fixaTabs, key = TableID)
		data.table::setkey(fixaRows, key = TableID)
		
		fixaIndex <- fixaTabs[fixaRows][,.(
			SeriesCode, 
			RowNumber, 
			LineDescription, 
			LineNumber, 
			ParentLineNumber, 
			Tier, 
			Path, 
			TableID,
			DatasetName,
			TableName, 
			ReleaseDate,
			NextReleaseDate,
			MetaDataUpdated = fixaMDU
		)]
		
		save(fixaIndex, file=paste0(beaMetadataStore, '/FixedAssets.RData'))
	})}


	#Regional data: Treated differently from National data 

	#Set "RegionalData"
	if('regionaldata' %in% tolower(datasetList)){try({
	
		rdatMDU <- metasetInfo[tolower(Datasetname) == 'regionaldata', MetaDataUpdated]
		rdatParam <- metaList$BEAAPI$Datasets$Parameter[[grep('regionaldata', tolower(metaList$BEAAPI$Datasets$Datasetname), fixed=T)]]
		#rbindlist(rdatParam[[1]])[ParamValue != 'NULL']
		rdatKeys <- as.data.table(rdatParam$Keycode$ParamValue[[1]])
		rdatKeys[, Parameter := 'Keycode']
		rdatFips <- as.data.table(rdatParam$GeoFIPS$ParamValue[[2]])
		rdatFips[, Parameter := 'GeoFIPS']
	
		rdatIndex <- rbindlist(list(rdatKeys, rdatFips), use.names = TRUE)
		rdatIndex[, DatasetName := 'RegionalData']
		rdatIndex[, MetaDataUpdated := rdatMDU]
	
		save(rdatIndex, file=paste0(beaMetadataStore, '/RegionalData.RData'))
	})}
	
	#Dataset "RegionalProduct"
	if('regionalproduct' %in% tolower(datasetList)){try({
		rprdMDU <- metasetInfo[tolower(Datasetname) == 'regionalproduct', MetaDataUpdated]
		rprdParams <- metaList$BEAAPI$Datasets$Parameters[[grep('regionalproduct', tolower(metaList$BEAAPI$Datasets$Datasetname), fixed=T)]]
		rprdParNms <- attributes(rprdParams)$names
		
		rprdPages <- rbindlist(rprdParams)[ParamValue != 'NULL', ParamValue]

		rprdIndex <- rbindlist(lapply(1:length(rprdPages), function(x){
			rprdDT <- as.data.table(rprdPages[[x]])
			rprdDT[, Parameter := rprdParNms[x]]
			return(rprdDT)
		}))

		rprdIndex[, DatasetName := 'RegionalProduct']
		rprdIndex[, MetaDataUpdated := rprdMDU]
		
		save(rprdIndex, file=paste0(beaMetadataStore, '/RegionalProduct.RData'))
	})}	
	
	#Dataset "RegionalIncome"
	if('regionalincome' %in% tolower(datasetList)){try({
		rincMDU <- metasetInfo[tolower(Datasetname) == 'regionalincome', MetaDataUpdated]
		rincParams <- metaList$BEAAPI$Datasets$Parameters[[grep('regionalincome', tolower(metaList$BEAAPI$Datasets$Datasetname), fixed=T)]]
		rincParNms <- attributes(rincParams)$names
		
		rincPages <- rbindlist(rincParams)[ParamValue != 'NULL', ParamValue]
	
		rincIndex <- rbindlist(lapply(1:length(rincPages), function(x){
			rincDT <- as.data.table(rincPages[[x]])
			rincDT[, Parameter := rincParNms[x]]
			return(rincDT)
		}))
		
		rincIndex[, DatasetName := 'RegionalIncome']
		rincIndex[, MetaDataUpdated := rincMDU]
	
	save(rincIndex, file=paste0(beaMetadataStore, '/RegionalIncome.RData'))
	})}
	
#	if(length(datasetList) > length(metasetInfo[, Datasetname])){
#		staleList <- datasetList[
#			!(tolower(datasetList) %in% tolower(metasetInfo[, Datasetname]))
#		]
#		message('beaR attempted to update metadata for the following dataset(s) which could not be returned from the API: ')
#		message(paste(
#			toupper(staleList), 
#			collapse = ', '
#		))
#		message('Removing stale data from local storage...')
##		return(staleList)
#	}# else {return(list())}
	

}
