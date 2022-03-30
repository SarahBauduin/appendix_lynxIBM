## Estimate correction factors for road mortality
## For resident and dispersing individuals
## Using inverse fitting (pattern-oriented modelling)

## See the script lynxIBM.Rmd to run the model once
## without calibration


# Try values for the parameters : corrFactorRes and corrFactorDisp
# Run 50 replicates each time
# Select the best values by reproducing 2 patterns
# - the total mortality rate (including road mortality) for residents = 0.24 (Breitenmoser-Würsten et al., 2007)
# - the total mortality rate (including road mortality) for dispersers = 0.47 (Breitenmoser-Würsten et al., 2007)
# 9 collisions in France in 2019 = the first year of simulation which allows collisions
pays <- shapefile("C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/inputs/countryBorders.shp")
Fr <- pays[pays$NAME == "France",]
rasterFr <- raster("C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/inputs/habMap.tif")
cellFr <- extract(rasterFr, Fr, cellnumbers = TRUE)
rasterFr[] <- 0
rasterFr[cellFr[[1]][,"cell"]] <- 1
mapFr_NLR <- createWorld(minPxcor = 1,
                          maxPxcor = NCOL(rasterFr),
                          minPycor = 1,
                          maxPycor = NROW(rasterFr),
                          data = values(rasterFr))

# First run
corrFactorResVal <- rep(c(0, 500, 1000, 2000, 3000), each = 5)
corrFactorDispVal <- rep(c(3000, 5000, 6250, 7000, 8000), 5)
# Model outputs to save
out <- cbind.data.frame(corrFactorRes = c(), corrFactorDisp = c(), replicate = c(),
                        mortResColl = c(), mortResOther = c(), mortDispColl = c(), 
                        mortDispOther = c(), nCollFr = c())

library(igraph)
library(SpaDES.core)
library(raster)

for(i in 1:length(corrFactorResVal)){
  
  # Where is located your appendix_lynxIBM repo
  pathRepo <- "C:/Users/sarah.bauduin/Documents/GitHub/" # change to your path
  
  # Define the paths
  moduleDir <- file.path(paste0(pathRepo, "appendix_lynxIBM/module"))
  inputDir <- file.path(paste0(pathRepo, moduleDir, "inputs")) %>% reproducible::checkPath(create = TRUE)
  outputDir <- file.path(paste0(pathRepo, moduleDir, "outputs"))
  cacheDir <- file.path(paste0(pathRepo, outputDir, "cache"))
  
  # Module parameters
  times <- list(start = 1, end = 51) # to run 50 years
  parameters <- list(
    .plotInitialTime = NA, # no plotting
    .plotInterval = NA,
    corrFactorRes = corrFactorResVal[i],
    corrFactorDisp = corrFactorDispVal[i],
    testON = TRUE # activate the inside-function tests 
  )
  
  # Input files
  # Habitat quality raster
  habMapSpaDES <- raster(paste0(pathRepo, "appendix_lynxIBM/module/inputs/habMap.tif"))
  # Collision probabilities raster
  collProbSpaDES <- raster(paste0(pathRepo, "appendix_lynxIBM/module/inputs/collProb.tif"))
  # List of 500 different initial population (SpatialPointsDataFrame)
  load(paste0(pathRepo, "appendix_lynxIBM/module/inputs/listLynxInitPop.RData"))
  popInitSpaDES <- listLynxInitPop[[sample(1:length(listLynxInitPop), 1)]] # sample one initial population
  # Population areas raster
  fourPopSpaDES <- raster(paste0(pathRepo, "appendix_lynxIBM/module/inputs/fourPop.tif"))
  
  modules <- list("lynxIBM")
  objects = c("habMapSpaDES", "collProbSpaDES", "popInitSpaDES", "fourPopSpaDES")
  paths <- list(
    cachePath = cacheDir,
    modulePath = moduleDir,
    inputPath = inputDir,
    outputPath = outputDir
  )
  
  # Initialize the module
  lynxIBMinit <- simInit(times = times, params = list(lynxIBM = parameters), modules = modules,
                         objects = objects, paths = paths)
  
  # Model outputs to save
  
  
  # Replicates 
  for(j in 1:10){
    lynxIBMrun <- spades(lynxIBMinit, debug = TRUE)
    
    # Outputs
    # Extract the outputs for parameter calibration
    # State of the population at the end of the previous time step
    # Calculate the number of residents and dispersers
    nRes <- unlist(lapply(lynxIBMrun$outputLynx, 
                   FUN = function(x){ifelse(NLcount(agents = x) > 0,
                                            NLcount(agents = NLwith(agents = x, var = "status", val = "res")),
                                            0)}))
    nDisp <- unlist(lapply(lynxIBMrun$outputLynx, 
                   FUN = function(x){ifelse(NLcount(agents = x) > 0,
                                            NLcount(agents = NLwith(agents = x, var = "status", val = "disp")),
                                            0)}))
    # Then calculate how many resident/dispersers died (from collisions and else)
    nResDeadColl <- unlist(lapply(lynxIBMrun$deadLynxColl, 
                   FUN = function(x){ifelse(NLcount(agents = x) > 0, 
                                            NLcount(agents = NLwith(agents = x, var = "status", val = "res")), 
                                            0)}))
    nResDeadNoColl <- unlist(lapply(lynxIBMrun$deadLynxNoColl, 
                           FUN = function(x){ifelse(NLcount(agents = x) > 0,
                                                    NLcount(agents = NLwith(agents = x, var = "status", val = "res")),
                                                    0)}))
    nDispDeadColl <- unlist(lapply(lynxIBMrun$deadLynxColl, 
                           FUN = function(x){ifelse(NLcount(agents = x) > 0,
                                                    NLcount(agents = NLwith(agents = x, var = "status", val = "disp")),
                                                    0)}))
    nDispDeadNoColl <- unlist(lapply(lynxIBMrun$deadLynxNoColl, 
                             FUN = function(x){ifelse(NLcount(agents = x) > 0,
                                                      NLcount(agents = NLwith(agents = x, var = "status", val = "disp")),
                                                      0)}))
    
    # How many collisions happened in France (residents and dispersers mixed) the second year of simulation
    if(NLcount(agents = lynxIBMrun$deadLynxColl[[2]]) != 0){
      collFr <- turtlesOn(world = mapFr_NLR, turtles = lynxIBMrun$deadLynxColl[[2]],
                          agents = NLwith(agents = patches(mapFr_NLR), world = mapFr_NLR, val = 1))
    } else {
      collFr <- noTurtles()
    }

    out <- rbind(out, 
                 cbind.data.frame(corrFactorRes = corrFactorResVal[i], 
                                  corrFactorDisp = corrFactorDispVal[i], 
                                  replicate = j,
                                  # Keep only the mortalities from the 2nd year
                                  # because there is no collisions simulated the 1st year
                                  # and not the last year 
                                  # because the final year does not run all the processes until the end
                                  mortResColl = mean(nResDeadColl[2:(length(nResDeadColl)-1)] / nRes[2:(length(nRes)-1)], na.rm = TRUE), 
                                  mortResOther = mean(nResDeadNoColl[2:(length(nResDeadNoColl)-1)] / nRes[2:(length(nRes)-1)], na.rm = TRUE), 
                                  mortDispColl = mean(nDispDeadColl[2:(length(nDispDeadColl)-1)] / nDisp[2:(length(nDisp)-1)], na.rm = TRUE), 
                                  mortDispOther = mean(nDispDeadNoColl[2:(length(nDispDeadNoColl)-1)] / nDisp[2:(length(nDisp)-1)], na.rm = TRUE), 
                                  nCollFr = NLcount(agents = collFr)
                 ))
    
    saveRDS(out, file = "out.rds")

  }

}
