This repository contains the necessary R code and data to reproduce the results of Parsons et al. 2022, Nest survival and cause-specific mortality in the San Clemente Bell’s Sparrow published in the Hournal of Field Ornithology.
https://doi.org/10.5751/JFO-00177-930402

This project monitored 110 San Clemente Bell's Sparrow nests over 8 years using video cameras and reports on nest survival, environmental correlates of nest survival, and cause specific mortality.


There is a single code file for this analysis: Bayesian_BESP_CSM.Rmd

This code manipulates the data into the needed format and contains three bayesian nest survival models. The first model contains no covariates and simply calculates average nest survival over the study period. The second model incorporates covariates to explore overall impacts of precipiation, nest substrate, and day of nesting season. The final model is a cause specifc mortality model that incorporates the same three covariates and explores how they impact three causes of mortality: native predators, nonnative predators, and other.

This code uses data from 5 different datafiles, each described in turn below. 

BESPsurv_proofed_SAM_padded.csv contains the daily status of each nest, whether the camera was functional that day, what stage the nest was in (laying, incubation, nestling), and the cause of nest failure specifically (Cause: e.g. to species of predator) and generally (Cause 2: e.g. to predator group). Each row represents 1 day of 1 nest.

precip.csv contains the total winter precipitation (mm) that fell the winter preceeding each breeding season. 

BESPSubstrate.csv contains the substrate each nest was located in. The "Substrate" column gives exact species and the "Substrate2" column generalizes substrates to the three categories used for analysis: Boxthorn, Sagebrush, and Other.

BreedingSeason.csv contains the start and end dates of each observed breeding season, defined as the first day an active nest was located and the last day an active nest was monitored.

NestInitiation.csv contains the estimated date that egg laying and incubation began for each nest based on observations and backdating. 
