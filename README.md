# Calibr

Calibr is an R package designed to calibrate species-rich population abundance data collected with different survey methods. It uses statistical models, the _Generalized Linear Model_ (GLM) and the _Generalized Linear Mixed Model_ (GLMM), to obtain standardization factors and convert all data to a single standard method.

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
The SMALL_UNPAIR data set provides an example of the necessary input data format.  
The headers are:  
GROUP: The species or other taxonomic grouping to split the analysis into.  
BLOCK: The space-time blocks to control for difference in abundance (e.g. "2017_South_Maui").  
REP: Data replicates (e.g. individual sites).  
METHOD: The sampling method used (has to be limited to 2 methods, standard and secondary).  
DENSITY: Species abundance for each REP (e.g. counts, density, biomass, etc.).  
PRESENCE: Presence (1) or absence (0) of a GROUP at each REP. This value can also be a fraction if there were subreplicates within replicates (e.g. if a species is seen on transect 1 but not on transect 2, PRESENCE will equal 0.5).  

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

For Windows users, the HOME directory is typically located at `C:/users/[USERNAME]/Documents/`. Where `[USERNAME]` is the system's user account name.

Two main files are generated in the output: "REP_summary"" which provides a breakdown of replicate sample sizes and "summary_table"" which presents the final results.

The headers for REP_summary are as follows:  
GROUP: Same as above.  
NREP_TOTAL: Total number of replicates in the data set (both methods combined).This excludes replicates from BLOCKs where a specific GROUP was not found.  
NREP_STD_METHOD: Total number of replicates  in the data set for the standard method.  
NREP_SEC_METHOD: Total number of replicates  in the data set for the secondary method.  
POSREP_TOTAL: Total number of replicates where a specific GROUP was observed (positive, non-zero count).  
POSREP_STD_METHOD: Total number of replicates from standard method where a specific GROUP was observed (positive, non-zero count).  
POSREP_SEC_METHOD: Total number of replicates from secondary method where a specific GROUP was observed (positive, non-zero count).  
  
The headers for summary_table are as follows:  
GROUP: Same as above.  
METHOD: Sampling method used.  
GCF.PRES: Gear calibration factor for presence-absence data.  
Convert probability of observation of secondary method following this equation: Prob_M1=inv.logit(logit(Prob_M2)-GCF.PRES)  
GCF.PRES_2.5: Lower bound of 95% probability interval of GCF.PRES.  
GCF.PRES_95: Upper bound of 95% probability interval of GCF.PRES.  
GCF.POS: Gear calibration factor for positive-only data.
Convert abundance metric of positive-only data for secondary method following this equation: Abund_M1=Abund_M2/GCF.POS  
GCF.POS_2.5: Lower bound of 95% probability interval of GCF.PRES.  
GCF.POS_95: Upper bound of 95% probability interval of GCF.PRES.  
PRES: Probability of observing a specific species for each method in the provided dataset.  
PRES.CAL: Calibrated probability of observing a species by method (used as a check on the success of the standardization procedure).  
POS: Abundance of a specific species for each method.  
POS.CAL: Calibrated abundance of a specific species for each method (used as a check on the success of the standardization procedure).  
OPUE: Obervation Per Unit Effort obtained by multiplying prob. of sighting and abundance (PRES x POS) in provided data set.  
OPUE.CAL: Obervation Per Unit Effort obtained by multiplying calibrated prob. of sighting and abundance (PRES.CAL x POS.CAL) in provided data set.  

