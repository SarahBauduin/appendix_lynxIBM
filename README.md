# appendix_lynxIBM

Files and code linked to the publication "Eurasian lynx populations in Western Europe: What prospects for the next 50 years?"

The "LICENSE" file gives the terms to reuse the model.

The "module" folder contains 3 folders: "inputs", "lynxIBM" and "outputs".
The folder "inputs" contains all the input files to run the lynx spatially explicit individual-based model (SE-IBM).
In this folder there are: 
- "collProb.tif" is a raster layer of the collision probabilities (see Appendix A of the publication).
- "countryBorders.shp" is a shapefile of the country borders (France, Germany, Switzerland which are the country we focused on)
- "fourPop.tif" is a raster layer of the population area (see Appendix A of the publication).
- "habMap.tif" is a raster layer of the different habitats for the lynx (see Appendix A of the publication).
- "listLynxInitPop.RData" is a list of 500 different initial lynx populations (see Appendix A of the publication).
In the folder "lynxIBM" there are:
- "lynxIBM.R" is the code of the lynx SE-IBM.
- "lynxIBM.Rmd" is the code to run the lynx SE-IBM. This code runs the model once and simulate lynx populations for 50 years.
In the folder "outputs" there are:
- a "cache" folder
- 200 simulation outputs of the SE-IBM.

The "analyzeResults.R" code takes the 200 simulation outputs from the "module" folder and analyze them, producing the results and figures in the publication.