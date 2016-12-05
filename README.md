# aiRpollution


[![DOI](https://zenodo.org/badge/21831/cole-brokamp/aiRpollution.svg)](https://zenodo.org/badge/latestdoi/21831/cole-brokamp/aiRpollution)

R package for assessing elemental PM exposure in Cincinnati based on land use random forest (LURF) models.

Install in `R` with `devtools::install_github('cole-brokamp/aiRpollution')`

This package uses a `R/sysdata.rda` file for all of the GIS sources.
After installation, the package will not work without the system data file.
Contact me (cole dot brokamp at gmail dot com) if you would like access to the system data file.

Find full details on the methodology [here](http://www.sciencedirect.com/science/article/pii/S1352231016309566):

Brokamp C, Jandarov R, Rao MB, Lemasters G, Ryan P. (2017). Exposure assessment models for elemental components of particulate matter in an urban environment: A comparison of regression and random forest approaches. *Atmospheric Environment*, 151, 1-11. doi:10.1016/j.atmosenv.2016.11.066
