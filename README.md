# Calibr

Calibr is an R package designed to calibrate species-rich population abundance data collected with different survey methods. It uses statistical models, such as the _Generalized Linear Model_ (GLM) and the _Generalized Linear Mixed Model_ (GLMM), to obtain standardization factors and convert all data to a single standard method.

## Github Disclaimer

_This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government._

## Installation

This is how to install the Calibr package.

```
install.packages("devtools")
library(devtools)
install_github("marcnadon/Calibr")
```

## Example 

This is how to run the Calibr package using the example dataset included in the package.

```
require(data.table) 
require(Calibr)

SET <- SMALL_UNPAIR
```
Note:
Look at the the SMALL_UNPAIR data set for proper way to format the data input.
The headers are:
GROUP: The species or other taxonomic grouping to split the analysis into.
BLOCK: The space-time blocks to control for difference in abundance (e.g. "2017_South_Maui").
REP: Data replicates (e.g. individual sites).
METHOD: The sampling method used (has to be limited to 2 methods, standard and secondary).
DENSITY: Species abundance for each REP (e.g. counts, density, biomass, etc.)
PRESENCE: Presence (1) or absence (0) of a GROUP at each REP. This value can also be a fraction if there were subreplicates within replicates
(e.g. if a species is seen on transect 1 but not on transect 2, PRESENCE will equal 0.5)

To Run the model and export results:
```
results <- run_calibr(SET,std_method="nSPC",stat_model="GLM")
export_results(results)

```
Note:
"std_method" defines the name of the standard method the secondary method's data need to be converted to.
"stat_model" defines the type of model use to run the calibration analyses (either "GLM" or "GLMM").

## Output

By default, ouptut from `export_results` will be written to a new `Calibr` subdirectory of the the users HOME directory. 

For Windows users, the HOME directory is typically located at `C:/users/[USERNAME]`. Where `[USERNAME]` is the system's user account name.
