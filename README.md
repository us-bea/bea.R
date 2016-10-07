# beaR
beaR: A library for use with BEA’s API.
To use this library, please [register for an API key](http://www.bea.gov/API/signup/index.cfm) first.

You can review [the API documentation](http://www.bea.gov/API/bea_web_service_api_user_guide.htm) for information about the parameters required for each dataset.

In order to use this library, users must have a local installation of the R programming language. Installation of this library will also install the libraries noted in the Depends and Imports sections of the DESCRIPTION file. 

Use of this library will result in data being stored on users' local machines. Specifically, local copies of BEA API metadata will be stored and automatically updated in the .libPaths() "/beaR/data" directory in order to improve performance of beaSearch.

Please contact "Developers [at] bea [dot] gov" with any questions.

This library serves two core purposes:

1.	To Extract/Transform/Load data [beaGet] from the BEA API as R-friendly formats in the user's workspace. Transformation done by default in beaGet is analogous to the format used in [BEA's iTables](http://www.bea.gov/itable/index.cfm), but this can be modified using beaGet's optional parameters.

2.	To enable the search of descriptive metadata [beaSearch].

Other features of the library exist mainly as intermediate methods or are in early stages of development.

#Installation

If you do not have the devtools library, please install:
```r
install.packages('devtools')
```

Then, install directly from this repo:
```r
library(devtools)
install_github('us-bea/beaR')
```

##Call the library
```r
library(devtools)
```

#Assign your key
In the examples below, we have already assumed that a key has been assigned to the variable "beaKey:"
```r
beaKey 	<- 'YOUR 36-DIGIT API KEY'
```
#beaSearch
This function returns a data.table containing descriptive metadata for all elements of BEA's National Income and Product Accounts (NIPA) and NI Underlying Detail data, as well as all elements of BEA's Regional Income and Product data.

The contents of this function are automatically undated using a new metadata component of BEA's API; as such, we recommend that you use it with your API key, and the first use of this function requires that you use your key or it will be unable to extract the metadata.

Syntax:
```r
beaSearch('personal consumption', beaKey)
```

You may also specify "asHtml = TRUE" to view in-browser:
```r
beaSearch('gross domestic', beaKey, asHtml = TRUE)
```

If you do not wish to automatically update the metadata (e.g., you have conducted a study using the search function), simply searching for the term without also passing your key to the function will do a search only using your locally stored version.

```r
beaSearch('tobacco')
```
However, *this approach is not advised.* If you would like to retain metadata for posterity, please copy it from the "beaR/data" area of your .libPaths() directory to local storage elsewhere on your machine; this will help prevent accidental overwrite, and will not interfere with the "freshness" of your searches.

#beaGet

The first parameter of this function is a list of your request parameters:
```r
beaSpecs <- list(
	'UserID' = beaKey ,
	'Method' = 'GetData',
	'datasetname' = 'NIPA',
	'TableID' = '66',
	'Frequency' = 'Q',
	'Year' = 'X',
	'ResultFormat' = 'json'
)
```

Setting asWide = FALSE gives results closest to the way they are actually returned and is a bit more like would traditionally define as "clean" data (every column is a variable, every row is an observation):
```r
beaLong <- beaGet(beaSpecs, asWide = FALSE)
```

asWide = TRUE and iTableStyle = TRUE by default, and this format looks the most like our iTables:
```r
beaPayload <- beaGet(beaSpecs)
```

Note that this is equivalent to:
```r
beaPayload <- bea2Tab(beaLong, asWide = TRUE)
```


To return in a format in which each column represents a series, set iTableStyle = FALSE.
This returns columns named with a concatenation of the descriptive column values, whereas rows are populated with numeric DataValues for each TimePeriod, and has one column named "TimePeriod" filled with dates.  
```r
beaStatTab <- beaGet(beaSpecs, iTableStyle = FALSE)
```
<aut: Andrea Julca>

# Disclaimer
The United States Department of Commerce (DOC) GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. DOC has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any claims against the Department of Commerce stemming from the use of its GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
