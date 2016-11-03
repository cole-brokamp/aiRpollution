# aiRpollution


[![DOI](https://zenodo.org/badge/21831/cole-brokamp/aiRpollution.svg)](https://zenodo.org/badge/latestdoi/21831/cole-brokamp/aiRpollution)

R package for assessing elemental PM exposure in Cincinnati based on land use random forest (LURF) models.

Install in `R` with `devtools::install_github('cole-brokamp/aiRpollution')`

This package uses a `R/sysdata.rda` file for all of the GIS sources.
After installation, the package will not work without the system data file. 
Contact me (cole dot brokamp at gmail dot com) if you would like access to the system data file.

Note: New LURF models have been created without using the deprivation index as one of the predictor variables; this will change predictions. The package was updated with these new models on November 3rd, 2016.

This package is based on work completed for my dissertation. For more details, see: http://colebrokamp.com/dissertation
