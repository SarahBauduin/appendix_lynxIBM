# R code to run a spatially-explicit individual-based model (IBM) for the Eurasian lynx

This is an appendix for the article entitled: "Modelling Eurasian lynx populations in Western Europe: What prospects for the next 50 years?".

The **LICENSE** file gives the terms to reuse the model.


The **module** folder contains 4 folders: **calibration**, **inputs**, **lynxIBM** and **outputs**, and 1 file **outputs_bestCal.RData**.

The folder **inputs** contains all the files used to calibrate the model and explore its sensitivity 
In this folder there are:
- a **calibration_phase1** folder in which there are 50 folders named **cal01**, **cal02**, **cal03** up until **cal50**. In each of these folders, there are 15 files, each one being a simulation output replicate using the parameters of the calibration. For example, the 15 files in the **cal01** folder have been run with the same calibration as defined for **cal01** from the first phase of calibration. 
- a **calibration_phase2** folder in which there are 50 folders named **cal01**, **cal02**, **cal03** up until **cal50**. In each of these folders, there are 15 files, each one being a simulation output replicate using the parameters of the calibration. For example, the 15 files in the **cal01** folder have been run with the same calibration as defined for **cal01** from the second phase of calibration. 
- **calibrIBM_phase1.RData** is dataframe which combine the mean value and 95% interval of the key simulation outputs to match from **valuesToCalibrate.txt** and the corresponded values computed from the 15 files of each calibration folder from the **calibration_phase1** folder.
- **calibrIBM_phase2.RData** is dataframe which combine the mean value and 95% interval of the key simulation outputs to match from **valuesToCalibrate.txt** and the corresponded values computed from the 15 files of each calibration folder from the **calibration_phase2** folder.
- **chooseParam.R** is the code to sample different sets of parameter for each calibration phase.
- **exploreSensitivity.R** is the code which explore the simulation outputs from **calibration_phase2** and explore the variability of the best calibration selected and the 49 others using key model outputs.
- **findBestCal.R** is the code to analyze each simulation output from each calibration for phase 1 and 2 to compute the mean value and 95% confidence intervals of the key outputs from **valuesToCalibrate.txt** to produce **calibrIBM_phase1.RData** and **calibrIBM_phase2.RData** and identify the calibration(s) which best reproduced (i.e., match the closest) these values.
- **valuesToCalibrate.txt** is a text document which lists the mean value and 95% interval of key simulation outputs to match to calibrate the model parameters.

The folder **inputs** contains all the input files to run the lynx spatially explicit individual-based model (SE-IBM).
In this folder there are: 
- **collProb.tif** is a raster layer of the collision probabilities (see Appendix A of the publication).
- **countryBorders.shp** is a shapefile of the country borders (France, Germany, Switzerland which are the country we focused on)
- **fourPop.tif** is a raster layer of the population area (see Appendix A of the publication).
- **habMap.tif** is a raster layer of the different habitats for the lynx (see Appendix A of the publication).
- **listLynxInitPop.RData** is a list of 500 different initial lynx populations (see Appendix A of the publication).

In the folder **lynxIBM** there are:
- **lynxIBM.R** is the code of the lynx SE-IBM.
- **lynxIBM.Rmd** is the code to run the lynx SE-IBM. This code runs the model once and simulate lynx populations for 50 years.

In the folder **outputs** there are:
- a **cache** folder
- **bestCalibration.txt** is a text document which lists the value used for the calibrated parameters to run the model to produce these outputs (i.e., values from the best calibration).
- 100 simulation outputs of the SE-IBM.

The file **outputs_bestCal.RData** encompasses multiple R objects of different key model outputs. For each object, there are the value computed for each of the 100 simulation outputs from the **outputs** folder.


The **analyzeResults.R** code takes the 100 simulation outputs from the **module** folder and analyze them, producing the results (key model outputs) and figures in the publication.

