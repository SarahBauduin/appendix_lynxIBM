# R code to run a spatially-explicit individual-based model (SE-IBM) for the Eurasian lynx

Data, scripts, code, and supplementary information regarding the article entitled: "Modelling Eurasian lynx populations in Western Europe: What prospects for the next 50 years?".

The **LICENSE** file gives the terms to reuse the model.

The **Supplementary Information_Bauduin_Modelling Eurasian lynx populations in Western Europe What prospects for the next 50 years** document contains the 4 Supplementary Information completing the article published in PCI Ecology.


The **code** folder contains 4 folders: **calibration**, **inputs**, **lynxIBM** and **outputs**, and 2 files **outputs_bestCal.RData** and **analyzeResults.R**.

The folder **calibration** contains all the files used to calibrate the model and explore its sensitivity. In this folder there are:
- a **calibration_phase1** folder in which there are 50 folders named **cal01**, **cal02**, **cal03**, ... up until **cal50**. In each of these folders, there are 15 files, each one being a simulation output replicate using the parameters of the calibration. For example, the 15 files in the **cal01** folder have been run with the same calibration as defined for **cal01** from the first phase of calibration. 
- a **calibration_phase2** folder in which there are 50 folders named **cal01**, **cal02**, **cal03**, ... up until **cal50**. In each of these folders, there are 15 files, each one being a simulation output replicate using the parameters of the calibration. For example, the 15 files in the **cal01** folder have been run with the same calibration as defined for **cal01** from the second phase of calibration. 
- **calibrIBM_phase1.RData** is dataframe which combines the mean value and 95% interval of the patterns to match from **valuesToCalibrate.txt** and the corresponded values computed from the 15 files of each calibration from the **calibration_phase1** folder.
- **calibrIBM_phase2.RData** is dataframe which combines the mean value and 95% interval of the patterns to match from **valuesToCalibrate.txt** and the corresponded values computed from the 15 files of each calibration from the **calibration_phase2** folder.
- **chooseParam.R** is the code to sample different sets of parameter for each calibration phase.
- **exploreSensitivity.R** is the code which explores the simulation outputs from **calibration_phase2** and explores the variability among all calibration by comparing pattern values from the best calibration selected and the 49 others.
- **findBestCal.R** is the code to analyze each simulation output from each calibration for phase 1 and 2 and to compute the mean value and 95% confidence intervals of the patterns from **valuesToCalibrate.txt** (producing **calibrIBM_phase1.RData** and **calibrIBM_phase2.RData**) and identify the calibration(s) which best reproduced (i.e., match the closest) these pattern values.
- **valuesToCalibrate.txt** is a text document which lists the mean value and 95% interval of the patterns to match to calibrate the model parameters.

The folder **inputs** contains all the input files to run the lynx spatially explicit individual-based model (SE-IBM).
In this folder there are: 
- **collProb.tif** is a raster layer of the collision probabilities (see Appendix A of the publication).
- **countryBorders.shp** is a shapefile of the country borders (France, Germany, Switzerland which are the country we focused on).
- **fourPop.tif** is a raster layer of the population area (see Appendix A of the publication).
- **habMap.tif** is a raster layer of the different habitats for the lynx (see Appendix A of the publication).
- **listLynxInitPop.RData** is a list of 500 different initial lynx populations (see Appendix A of the publication).

In the folder **lynxIBM** there are:
- **lynxIBM.R** is the code of the lynx SE-IBM.
- **lynxIBM.Rmd** is the code to run the lynx SE-IBM. This code runs the model once and simulate lynx populations for 50 years.

In the folder **outputs** there are:
- a **cache** folder.
- **bestCalibration.txt** is a text document which lists the value used for the calibrated parameters to run the model to produce these outputs (i.e., values from the best calibration).
- 100 simulation outputs of the SE-IBM.

The **analyzeResults.R** code takes the 100 simulation outputs from the **module** folder and analyze them (producing **outputs_bestCal.RData**), and computing the results and figures in the publication.

The file **outputs_bestCal.RData** assembles multiple R objects of different key model outputs. For each object, there are the value computed for each of the 100 simulation outputs from the **outputs** folder.


