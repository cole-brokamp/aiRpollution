# aiRpollution
R package for assessing elemental PM exposure in Cincinnati

Install in `R` with `devtools::install_github('cole-brokamp/aiRpollution')`

This package uses a `R/sysdata.rda` file for all of the GIS sources.
After installation, the package will not work without the system data file. 
Contact me (cole dot brokamp at gmail dot com) if you would like access to the system data file.

Note: This package does not censor the predictions at any upper limit. It may be useful to censor some of the linear model predictions 
at 10 times the observed maximum.
