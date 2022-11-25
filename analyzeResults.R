library(NetLogoR)
library(colorspace)
library(ggplot2)
library(data.table)
library(Rmisc)
library(raster)
library(mapview)
library(viridis)
library(wesanderson)
library(rgeos)

#############################
## Analyze the simulations ##
#############################
# Read all simulated files
pathFiles <- "appendix_lynxIBM/module/outputs"
listSim <- list.files(pathFiles)[2:201] # remove the first file (cache)
nSim <- length(listSim)
lastYear <- 50

# Play with the color from the palet used by ggplot
gg_color <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#######################
## Model calibration ##
#######################
### Resulting mortality rates
rCollAll <- c()
rCollRes <- c()
rCollDisp <- c()
rNoCollAll <- c()
rNoCollRes <- c()
rNoCollDisp <- c()

for(popName in c("Alps", "Jura", "Vosges-Palatinate", "BlackForest")){
  
  # Realized mortality rates for residents and dispersers by collision or otherwise
  # Starts at year 10 (before = burn-in)
  deathLynx <- cbind(repSim = rep(1:nSim, each = lastYear-9), year = rep(10:lastYear, nSim), 
                     nCollAll = rep(0, (lastYear-9)*nSim), nCollRes = rep(0, (lastYear-9)*nSim), nCollDisp = rep(0, (lastYear-9)*nSim),
                     nNoCollAll = rep(0, (lastYear-9)*nSim), nNoCollRes = rep(0, (lastYear-9)*nSim), nNoCollDisp = rep(0, (lastYear-9)*nSim),
                     nAll = rep(0, (lastYear-9)*nSim), nRes = rep(0, (lastYear-9)*nSim), nDisp = rep(0, (lastYear-9)*nSim), 
                     rCollAll = rep(0, (lastYear-9)*nSim), rCollRes = rep(0, (lastYear-9)*nSim), rCollDisp = rep(0, (lastYear-9)*nSim), 
                     rNoCollAll = rep(0, (lastYear-9)*nSim), rNoCollRes = rep(0, (lastYear-9)*nSim), rNoCollDisp = rep(0, (lastYear-9)*nSim))
  
  for(i in 1:length(listSim)){ # for each simulation run
    load(paste0(pathFiles, "/", listSim[i]))
    
    for(y in 10:(lastYear)){
      
      if(NLcount(lynxIBMrun$deadLynxColl[[y]]) == 0){
        deadLynxColl <- noTurtles()
      } else {
        deadLynxColl <- NLwith(agents = lynxIBMrun$deadLynxColl[[y]], var = "pop", val = popName)
      }
      if(NLcount(lynxIBMrun$deadLynxNoColl[[y]] & NLcount(lynxIBMrun$deadOldLynx[[y]])) == 0){ # add the old lynx
        deadLynxNoColl <- noTurtles()
      } else {
        deadLynxNoColl <- NLwith(agents = turtleSet(lynxIBMrun$deadLynxNoColl[[y]], lynxIBMrun$deadOldLynx[[y]]), var = "pop", val = popName)
      }
      if(NLcount(lynxIBMrun$outputLynx[[y]]) == 0){
        outputLynx <- noTurtles()
      } else {
        outputLynx <- NLwith(agents = lynxIBMrun$outputLynx[[y]], var = "pop", val = popName)
      }
      
      # Collisions - All
      nCollAll <- NLcount(agents = deadLynxColl)

      
      # Deaths other than by collisions - All
      nNoCollAll <- NLcount(agents = deadLynxNoColl)

      
      # Collisions - Residents
      if(NLcount(deadLynxColl) == 0){
        nCollRes <- 0
      } else {
        nCollRes <- NLcount(agents = NLwith(agents = deadLynxColl, var = "status", val = "res"))
      }
      
      # Deaths other than by collisions - Residents
      if(NLcount(deadLynxNoColl) == 0){
        nNoCollRes <- 0
      } else {
        nNoCollRes <- NLcount(agents = NLwith(agents = deadLynxNoColl, var = "status", val = "res"))
      }
      
      # Collisions - Dispersers
      if(NLcount(deadLynxColl) == 0){
        nCollDisp <- 0
      } else {
        nCollDisp <- NLcount(agents = NLwith(agents = deadLynxColl, var = "status", val = "disp"))
      }
      
      # Deaths other than by collisions - Dispersers
      if(NLcount(deadLynxNoColl) == 0){
        nNoCollDisp <- 0
      } else {
        nNoCollDisp <- NLcount(agents = NLwith(agents = deadLynxNoColl, var = "status", val = "disp"))
      }
      
      
      # Number of individuals at the beginning of the yearly time step
      if(NLcount(outputLynx) != 0){
        nAll <- NLcount(agents = outputLynx)
        nRes <- NLcount(agents = NLwith(agents = outputLynx,
                                        var = "status", val = "res"))
        nDisp <- NLcount(agents = NLwith(agents = outputLynx,
                                         var = "status", val = "disp"))
      } else {
        nAll <- 0
        nRes <- 0
        nDisp <- 0
      }
      
      # Mortality rates
      deathLynx[deathLynx[, "year"] == y & deathLynx[, "repSim"] == i, "nCollAll"] <- nCollAll
      deathLynx[deathLynx[, "year"] == y & deathLynx[, "repSim"] == i, "nCollRes"] <- nCollRes
      deathLynx[deathLynx[, "year"] == y & deathLynx[, "repSim"] == i, "nCollDisp"] <- nCollDisp
      deathLynx[deathLynx[, "year"] == y & deathLynx[, "repSim"] == i, "nNoCollAll"] <- nNoCollAll
      deathLynx[deathLynx[, "year"] == y & deathLynx[, "repSim"] == i, "nNoCollRes"] <- nNoCollRes
      deathLynx[deathLynx[, "year"] == y & deathLynx[, "repSim"] == i, "nNoCollDisp"] <- nNoCollDisp
      deathLynx[deathLynx[, "year"] == y & deathLynx[, "repSim"] == i, "nAll"] <- nAll
      deathLynx[deathLynx[, "year"] == y & deathLynx[, "repSim"] == i, "nRes"] <- nRes
      deathLynx[deathLynx[, "year"] == y & deathLynx[, "repSim"] == i, "nDisp"] <- nDisp
      deathLynx[deathLynx[, "year"] == y & deathLynx[, "repSim"] == i, "rCollAll"] <- nCollAll / nAll
      deathLynx[deathLynx[, "year"] == y & deathLynx[, "repSim"] == i, "rCollRes"] <- nCollRes / nRes
      deathLynx[deathLynx[, "year"] == y & deathLynx[, "repSim"] == i, "rCollDisp"] <- nCollDisp / nDisp
      deathLynx[deathLynx[, "year"] == y & deathLynx[, "repSim"] == i, "rNoCollAll"] <- nNoCollAll / nAll
      deathLynx[deathLynx[, "year"] == y & deathLynx[, "repSim"] == i, "rNoCollDisp"] <- nNoCollDisp / nDisp
      deathLynx[deathLynx[, "year"] == y & deathLynx[, "repSim"] == i, "rNoCollRes"] <- nNoCollRes / nRes
      
    }
    print(i)
  }
  
  
  # We need to remove the time when there were no more individuals
  # and remove the first year when we removed the mortality
  rCollAll <- c(rCollAll, mean(deathLynx[deathLynx[, "nAll"] != 0, "rCollAll"], na.rm = TRUE))
  rCollRes <- c(rCollRes, mean(deathLynx[deathLynx[, "nRes"] != 0, "rCollRes"], na.rm = TRUE))
  rCollDisp <- c(rCollDisp, mean(deathLynx[deathLynx[, "nDisp"] != 0, "rCollDisp"], na.rm = TRUE))
  rNoCollAll <- c(rNoCollAll, mean(deathLynx[deathLynx[, "nAll"] != 0, "rNoCollAll"], na.rm = TRUE))
  rNoCollRes <- c(rNoCollRes, mean(deathLynx[deathLynx[, "nRes"] != 0, "rNoCollRes"], na.rm = TRUE))
  rNoCollDisp <- c(rNoCollDisp, mean(deathLynx[deathLynx[, "nDisp"] != 0, "rNoCollDisp"], na.rm = TRUE))
  
}

# To calculate the mean over the populations, we remove the BlackForest
# because there are too few individuals and it bias the mean
mean(rNoCollAll[1:3])
mean(rNoCollRes[1:3])
mean(rNoCollDisp[1:3])
mean(rCollAll[1:3])
mean(rCollRes[1:3])
mean(rCollDisp[1:3])


### Reproduction rate
pRepro <- numeric()

for(i in 1:length(listSim)){ # for each simulation run
  load(paste0(pathFiles, "/", listSim[i]))
  
  resInd <- lynxIBMrun$resInd # resident individuals
  nKittyBorn <- lynxIBMrun$nKittyBorn # number of kittens produced per reproducing female
  
  nFemRes <- rep(0, length(resInd) - 10) # resInd = 51 elements
  nKitty <- rep(0, length(resInd) - 10)
  
  for(j in 10:(length(resInd) - 1)){ # remove burn-in
    # Number of resident females
    nFemRes[j-9] <- NLcount(agents = NLwith(agents = resInd[[j]], var = "sex", val = "F"))
  }
  
  for(j in 10:length(nKittyBorn)){ # remove burn-in
    # Number of females which actually produced newborns
    nKitty[j-9] <- length(which(nKittyBorn[[j]] != 0))
  }
  
  # Reproduction rate
  pRepro <- c(pRepro, nKitty / nFemRes)
  print(i)
}

summary(pRepro)


#############################
## Population growth rates ##
#############################
# Prepare the table to store the individuals per population
popAlps <- cbind(repSim = rep(1:nSim, each = lastYear+1), year = rep(1:(lastYear+1), nSim), 
                 nInd = rep(0, (lastYear+1)*nSim))
popJura <- popAlps
popVP <- popAlps
popBF <- popAlps

for(i in 1:length(listSim)){ # for each simulation run
  load(paste0(pathFiles, "/", listSim[i]))
  for(y in 1:(lastYear+1)){
    
    indAlps <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$outputLynx[[y]],
                         agents = NLwith(agents = patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 1))
    popAlps[popAlps[, "year"] == y & popAlps[, "repSim"] == i, "nInd"] <- NLcount(indAlps) 
    
    indJura <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$outputLynx[[y]],
                         agents = NLwith(agents = patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 2))
    popJura[popJura[, "year"] == y & popJura[, "repSim"] == i, "nInd"] <- NLcount(indJura) 
   
    indVosPal <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$outputLynx[[y]],
                           agents = NLwith(agents = patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 3))
    popVP[popVP[, "year"] == y & popVP[, "repSim"] == i, "nInd"] <- NLcount(indVosPal) 
        
    indBF <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$outputLynx[[y]],
                         agents = NLwith(agents = patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 4))
    popBF[popBF[, "year"] == y & popBF[, "repSim"] == i, "nInd"] <- NLcount(indBF)
  }
  print(i)
}

allPop <- rbind(cbind.data.frame(popAlps, pop = "Alps"), cbind.data.frame(popJura, pop = "Jura"),
                cbind.data.frame(popVP, pop = "Vosges-Palatinate"), cbind.data.frame(popBF, pop = "Black Forest"))

# Calculate the growth rate
nInt_tMinus1 <- allPop$nInd
nInt_tMinus1 <- c(NA, nInt_tMinus1[1:length(nInt_tMinus1) - 1])
# Replace all nInt_tMinus1 by NA for year 1
allPop <- cbind(allPop, nInt_tMinus1)
allPop[allPop$year == 1, "nInt_tMinus1"] <- NA
allPop$r <- allPop$nInd / allPop$nInt_tMinus1
allPop[is.infinite(allPop$r), "r"] <- NA

# Calculate the mean and 95% confidence intervals of the growth rates per year and per population
allPopSum <- summarySE(allPop, measurevar = "r", groupvars = c("year", "pop"), na.rm = TRUE)

# Plot
colnames(allPopSum)[2] <- "Populations"
ggplot(allPopSum, aes(x=year, y=r, colour=Populations)) + 
  geom_ribbon(aes(ymin=r-ci, ymax=r+ci, x=year, fill=Populations),alpha = 0.3) +
  geom_line() +
  geom_point() +
  coord_cartesian(ylim=c(0, 2)) +
  annotate("rect", xmin = -Inf, xmax = 10, ymin = -Inf, ymax = Inf, alpha = .7) +
  labs(x ="Years simulated", y = "Population growth rates")


##################
## Lynx density ##
##################
# Load a map from any simulation to have the study area
load(paste0(pathFiles, "/", listSim[1]))
mapLynxAbund <- NLset(world = lynxIBMrun$habitatMap, agents = patches(lynxIBMrun$habitatMap), val = 0) # empty a map

for(i in 1:length(listSim)){ # for each simulation run
  load(paste0(pathFiles, "/", listSim[i]))
  # Select adult resident lynx
  if(NLcount(lynxIBMrun$outputLynx[[lastYear]] != 0)){
    resLynx <- NLwith(agents = lynxIBMrun$outputLynx[[lastYear]], var = "status", val = "res") # residents lynx
    resAdults <- NLwith(agents = resLynx, var = "age", val = 1:20) # adult are between 1 year old (2nd year of life, not kitten of the year) and 20 years (oldest age possible)
    patchesLynx <- patchHere(world = lynxIBMrun$habitatMap, turtles = resAdults) # retrieve the occupied cells
    patchesLynxCnt <- ddply(as.data.frame(patchesLynx),. (pxcor, pycor), nrow)
    mapLynxAbund <- NLset(world = mapLynxAbund, agents = cbind(pxcor = patchesLynxCnt$pxcor, pycor = patchesLynxCnt$pycor),
                          val = of(world = mapLynxAbund, agents = cbind(pxcor = patchesLynxCnt$pxcor, pycor = patchesLynxCnt$pycor)) + patchesLynxCnt$V1)
  } 
  print(i)
}

# Transfer the data from the worldMatrix map to a raster format
mapLynxAbundRas <- raster("appendix_lynxIBM/module/inputs/habMap.tif")
# Rescale the value to obtain a mean over all simulations
mapLynxAbundRas[] <- of(world = mapLynxAbund, agents = patches(mapLynxAbund)) / nSim
# The map before was of 1 km2, transform it into 100 km2
mapLynxAbund100km2 <- aggregate(mapLynxAbundRas, fact = 10, fun = sum)
# Give NA where there were no lynx simulated
mapLynxAbund100km2[mapLynxAbund100km2 == 0] <- NA
# Plot the density with the country borders
plot(mapLynxAbund100km2)
pays <- shapefile("appendix_lynxIBM/module/inputs/countryBorders.shp")
plot(pays, add = TRUE)


############################################################
## Movement from native population to establish elsewhere ##
############################################################
# Calculate the movement between population
# when an individual become resident in another population that the one is was born in
movePop <- cbind(repSim = rep(1:nSim, each = lastYear), year = rep(1:lastYear, nSim), 
                 AtoJ = rep(0, lastYear*nSim), AtoBF = rep(0, lastYear*nSim), AtoVP = rep(0, lastYear*nSim),
                 JtoA = rep(0, lastYear*nSim), JtoBF = rep(0, lastYear*nSim), JtoVP = rep(0, lastYear*nSim),
                 BFtoA = rep(0, lastYear*nSim), BFtoJ = rep(0, lastYear*nSim), BFtoVP = rep(0, lastYear*nSim),
                 VPtoA = rep(0, lastYear*nSim), VPtoJ = rep(0, lastYear*nSim), VPtoBF = rep(0, lastYear*nSim))

for(i in 1:length(listSim)){ # for each simulation run
  load(paste0(pathFiles, "/", listSim[i]))
  
  for(y in 1:lastYear){
    
    # Identify the residents in the different populations
    resAlps <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$resLynx[[y]],
                         agents = NLwith(agents = patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 1))
    resJura <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$resLynx[[y]],
                         agents = NLwith(agents = patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 2))
    resVP <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$resLynx[[y]],
                       agents = NLwith(agents = patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 3))
    resBF <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$resLynx[[y]],
                       agents = NLwith(agents = patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 4))
    
    # Where these residents were from?
    if(NLcount(resAlps) != 0){
      whoResAlps <- of(agents = resAlps, var = "pop")
      movePop[movePop[, "year"] == y & movePop[, "repSim"] == i, "JtoA"] <- length(which(whoResAlps == "Jura")) # established in the Alps but coming from the Jura
      movePop[movePop[, "year"] == y & movePop[, "repSim"] == i, "VPtoA"] <- length(which(whoResAlps == "Vosges-Palatinate")) # established in the Alps but coming from the Vosges-Palatinate
      movePop[movePop[, "year"] == y & movePop[, "repSim"] == i, "BFtoA"] <- length(which(whoResAlps == "BlackForest")) # established in the Alps but coming from the Black Forest
    }
    if(NLcount(resJura) != 0){
      whoResJura <- of(agents = resJura, var = "pop")
      movePop[movePop[, "year"] == y & movePop[, "repSim"] == i, "AtoJ"] <- length(which(whoResJura == "Alps")) # established in the Jura but coming from the Alps
      movePop[movePop[, "year"] == y & movePop[, "repSim"] == i, "VPtoJ"] <- length(which(whoResJura == "Vosges-Palatinate")) # established in the Jura but coming from the Vosges-Palatinate
      movePop[movePop[, "year"] == y & movePop[, "repSim"] == i, "BFtoJ"] <- length(which(whoResJura == "BlackForest")) # established in the Jura but coming from the Black Forest
    }
    if(NLcount(resVP)){
      whoResVP <- of(agents = resVP, var = "pop")
      movePop[movePop[, "year"] == y & movePop[, "repSim"] == i, "AtoVP"] <- length(which(whoResVP == "Alps")) # established in the Vosges-Palatinate but coming from the Alps
      movePop[movePop[, "year"] == y & movePop[, "repSim"] == i, "JtoVP"] <- length(which(whoResVP == "Jura")) # established in the Vosges-Palatinate but coming from the Jura
      movePop[movePop[, "year"] == y & movePop[, "repSim"] == i, "BFtoVP"] <- length(which(whoResVP == "BlackForest")) # established in the Vosges-Palatinate but coming from the Black Forest
    }
    if(NLcount(resBF)){
      whoResBF <- of(agents = resBF, var = "pop")
      movePop[movePop[, "year"] == y & movePop[, "repSim"] == i, "AtoBF"] <- length(which(whoResBF == "Alps")) # established in the Black Forest but coming from the Alps
      movePop[movePop[, "year"] == y & movePop[, "repSim"] == i, "JtoBF"] <- length(which(whoResBF == "Jura")) # established in the Black Forest but coming from the Jura
      movePop[movePop[, "year"] == y & movePop[, "repSim"] == i, "VPtoBF"] <- length(which(whoResBF == "Vosges-Palatinate")) # established in the Black Forest but coming from the Vosges-Palatinate
    }
  }
  print(i)
}

# Summarize the data
movePopLongDT <- melt(setDT(as.data.frame(movePop)), id = c("repSim", "year"))
# Compute the cumulative sum of the movement over the years per simulation and per variable (=pair of populations with a direction)
movePopLongDT[, Cum.Sum := cumsum(value), by=list(repSim, variable)] 
# Compute the mean and 95% confidence intervals per year and per variable
movePopLongDTSum <- summarySE(as.data.frame(movePopLongDT), measurevar = "Cum.Sum", groupvars = c("year","variable"))
# Remove the variable where there was no movement simulated between the two population
movePopLongDTSum2 <- movePopLongDTSum[movePopLongDTSum$variable %in% movePopLongDTSum[movePopLongDTSum$year == lastYear
                                                                                      & movePopLongDTSum$Cum.Sum >= 0.05,"variable"],]
movePopLongDTSum2$variable <- as.character(movePopLongDTSum2$variable)
# Rename the variable to display in the legend of the plot
colnames(movePopLongDTSum2)[2] <- "Populations"
movePopLongDTSum2[movePopLongDTSum2$Populations == "AtoJ", "Populations"] <- "Alps to Jura"
movePopLongDTSum2[movePopLongDTSum2$Populations == "AtoVP", "Populations"] <- "Alps to Vosges-Palatinate"
movePopLongDTSum2[movePopLongDTSum2$Populations == "AtoBF", "Populations"] <- "Alps to Black Forest"
movePopLongDTSum2[movePopLongDTSum2$Populations == "JtoA", "Populations"] <- "Jura to Alps"
movePopLongDTSum2[movePopLongDTSum2$Populations == "JtoVP", "Populations"] <- "Jura to Vosges-Palatinate"
movePopLongDTSum2[movePopLongDTSum2$Populations == "JtoBF", "Populations"] <- "Jura to Black Forest"
movePopLongDTSum2[movePopLongDTSum2$Populations == "VPtoA", "Populations"] <- "Vosges-Palatinate to Alps"
movePopLongDTSum2[movePopLongDTSum2$Populations == "VPtoJ", "Populations"] <- "Vosges-Palatinate to Jura"
movePopLongDTSum2[movePopLongDTSum2$Populations == "VPtoBF", "Populations"] <- "Vosges-Palatinate to Black Forest"
movePopLongDTSum2[movePopLongDTSum2$Populations == "BFtoA", "Populations"] <- "Black Forest to Alps"
movePopLongDTSum2[movePopLongDTSum2$Populations == "BFtoJ", "Populations"] <- "Black Forest to Jura"
movePopLongDTSum2[movePopLongDTSum2$Populations == "BFtoVP", "Populations"] <- "Black Forest to Vosges-Palatinate"
# Organize the factor into orders
movePopLongDTSum2$Populations <- factor(movePopLongDTSum2$Populations, levels =c(
  "Alps to Jura","Alps to Vosges-Palatinate","Alps to Black Forest",
  "Jura to Alps","Jura to Vosges-Palatinate","Jura to Black Forest",
  "Vosges-Palatinate to Alps","Vosges-Palatinate to Jura","Vosges-Palatinate to Black Forest",
  "Black Forest to Alps","Black Forest to Jura","Black Forest to Vosges-Palatinate"
))

# Plot
colRainbow <- rainbow_hcl(n = length(unique(movePopLongDTSum2$Populations)), c = 130, l = 70)

ggplot(movePopLongDTSum2, aes(x = year, y = Cum.Sum, colour = Populations)) + 
  geom_ribbon(aes(ymin = Cum.Sum-ci, ymax = Cum.Sum+ci, x = year, fill = Populations), alpha = 0.3) +
  geom_line() +
  geom_point() +
  labs(x= "Years simulated", y = "Cumulative sum of established individuals in their non-native population", color = "Populations") +
  scale_color_manual(values = colRainbow) +
  scale_fill_manual(values = colRainbow) +
  annotate("rect", xmin = -Inf, xmax = 10, ymin = -Inf, ymax = Inf, alpha = .7)


#########################
## Territory occupancy ##
#########################
# Load a map from any simulation to have the study area
load(paste0(pathFiles, "/", listSim[1]))
terrOccMap <- NLset(world = lynxIBMrun$outputTerrMap[[1]], agents = patches(lynxIBMrun$outputTerrMap[[1]]), val = 0)  # empty a map

for(i in 1:length(listSim)){ # for each simulation run
  load(paste0(pathFiles, "/", listSim[i]))
  terrOccMapSim <- lynxIBMrun$outputTerrMap[[lastYear]]
  valTerrOccMapSim <- of(world = terrOccMapSim, agents = patches(terrOccMapSim))
  # Transform the value of the territories into 0 and 1 for presence or abscence of a territory
  valTerrOccMapSim[!is.na(valTerrOccMapSim)] <- 1
  valTerrOccMapSim[is.na(valTerrOccMapSim)] <- 0
  terrOccMap <- NLset(world = terrOccMap, agents = patches(terrOccMap),
                      val = of(world = terrOccMap, agents = patches(terrOccMap)) + valTerrOccMapSim)
  print(i)
}

# Transfer the data from the worldMatrix map to a raster format
terrOccMapRas <- raster("appendix_lynxIBM/module/inputs/habMap.tif")
# Rescale the value to obtain a mean over all simulations
terrOccMapRas[] <- of(world = terrOccMap, agents = patches(terrOccMap)) / nSim
# Give NA where there were no female lynx territory simulated
terrOccMapRas[terrOccMapRas == 0] <- NA
# Plot the territory occupancy with the country borders
plot(terrOccMapRas)
pays <- shapefile("appendix_lynxIBM/module/inputs/countryBorders.shp")
plot(pays, add = TRUE)

# Include the non-used breeding habitat
habMapSpaDES <- raster("appendix_lynxIBM/module/inputs/habMap.tif")
habMapSpaDES[habMapSpaDES %in% c(0, 1, 2, 3)] <- NA
habMapSpaDES[habMapSpaDES == 4] <- 1
habMapSpaDES[!is.na(terrOccMapRas)] <- NA
plot(habMapSpaDES)
habPol <- rasterToPolygons(habMapSpaDES)
# Adapt the colorscale
plot(terrOccMapRas, col=wes_palette("Zissou1", 100, type = "continuous"))
plot(habPol, col = "gray80", border = NA, add = TRUE)
plot(pays, add = TRUE)


########################
## Dispersal distance ##
########################
dispDistList <- list()
popList <- 1
# Use a raster to transfer the territories on it to extract the centroid
habMapSpaDES <- raster("appendix_lynxIBM/module/inputs/habMap.tif")

for(popName in c("Alps", "Jura", "Vosges-Palatinate", "BlackForest")){
  dispDist <- c()
  
for(i in 1:length(listSim)){ # for each simulation run
  load(paste0(pathFiles, "/", listSim[i]))
  
  bornInd <- cbind.data.frame(xcor = c(), ycor = c(), who = c())
  resInd <- cbind.data.frame(xcor2 = c(), ycor2 = c(), who = c())
  
  for(y in 10:(lastYear - 1)){ 
    
    # Territories where lynx were born
    if(NLcount(agents = NLwith(agents = lynxIBMrun$bornLynx[[y]],var = "pop", val = popName)) > 0){
      # Territory centroids
      habMapSpaDES[] <- lynxIBMrun$outputTerrMap[[y]] # transfer the territory number from the worldMatrix to a raster
      terrPol <- rasterToPolygons(habMapSpaDES, dissolve = TRUE)
      spPoints <- gCentroid(terrPol, byid = TRUE)
      centroidTerr <- cbind.data.frame(xcor = spPoints@coords[,1], ycor = spPoints@coords[,2], 
                                       terrNum = terrPol@data)
      # Born individuals
      terrBornInd <- of(world = lynxIBMrun$outputTerrMap[[y]],
                        agents = patchHere(world = lynxIBMrun$outputTerrMap[[y]], 
                                           turtles = NLwith(agents=lynxIBMrun$bornLynx[[y]],var = "pop",val = popName)))
      terrBornInd2 <- merge(as.data.frame(terrBornInd), centroidTerr, by.x = "terrBornInd", by.y = "habMap")
      terrBornInd2 <- terrBornInd2[match(terrBornInd, terrBornInd2[,"terrBornInd"]),]
      bornInd <- rbind(bornInd, 
                       cbind.data.frame(xcor = terrBornInd2[,"xcor"], ycor = terrBornInd2[,"ycor"],
                                        who = of(agents = NLwith(agents=lynxIBMrun$bornLynx[[y]],var = "pop",val = popName), var = "who")))
      
    }
    # Territories of resident lynx
    if(NLcount(agents = lynxIBMrun$resLynx[[y]]) > 0 & NLcount(agents = lynxIBMrun$outputLynx[[y + 1]]) > 0){
      
      # Separate male and female territories
      # Females
      resFem <- NLwith(agents = lynxIBMrun$resLynx[[y]], var = "sex", val = "F")
      # Residents that survived the year when they became resident
      resFem <- NLwith(agents = resFem, var = "who", 
                       val = of(agents = lynxIBMrun$outputLynx[[y + 1]], var = "who"))
      if(NLcount(resFem) > 0){
        # Territory centroids
        habMapSpaDES[] <- lynxIBMrun$outputTerrMap[[y + 1]] # transfer the territory number from the worldMatrix to a raster
        terrPol <- rasterToPolygons(habMapSpaDES, dissolve = TRUE)
        spPoints <- gCentroid(terrPol, byid = TRUE)
        centroidTerr <- cbind.data.frame(xcor = spPoints@coords[,1], ycor = spPoints@coords[,2], 
                                         terrNum = terrPol@data)
        # Resident females
        terrResFem <- of(world = lynxIBMrun$outputTerrMap[[y + 1]],
                         agents = patchHere(world = lynxIBMrun$outputTerrMap[[y + 1]], turtles = resFem))
        terrResFem2 <- merge(as.data.frame(terrResFem), centroidTerr, by.x = "terrResFem", by.y = "habMap")
        terrResFem2 <- terrResFem2[match(terrResFem, terrResFem2[,"terrResFem"]),]
        resInd <- rbind(resInd, 
                        cbind.data.frame(xcor2 = terrResFem2[,"xcor"], ycor2 = terrResFem2[,"ycor"],
                                         who = of(agents = resFem, var = "who")))
        
      }
      # Males
      resMal <- NLwith(agents = lynxIBMrun$resLynx[[y]], var = "sex", val = "M")
      # Residents that survived the year when they became resident
      # and who are still resident (their female(s) did not all die)
      resMal <- NLwith(agents = resMal, var = "who", 
                       val = of(agents = NLwith(agents = lynxIBMrun$outputLynx[[y + 1]],
                                                var = "status", val = "res"),
                                var = "who"))
      if(NLcount(resMal) > 0){
        # Male territories are a combination of several female territories
        for(eachMale in of(agents = resMal, var = "who")){
          allFem <- NLwith(agents = lynxIBMrun$outputLynx[[y + 1]], var = "maleID", val = eachMale)
          # Retrieve the territories of these females (their who numbers)
          # Territory centroids
          habMapSpaDES[] <- lynxIBMrun$outputTerrMap[[y + 1]] # transfer the territory number from the worldMatrix to a raster
          habMapSpaDES[!habMapSpaDES %in% of(agents = allFem, var = "who")] <- NA
          terrPol <- rasterToPolygons(habMapSpaDES, dissolve = TRUE)
          spPoints <- gCentroid(terrPol)
          centroidTerr <- cbind.data.frame(xcor = spPoints@coords[,1], ycor = spPoints@coords[,2], 
                                           terrNum = eachMale)
          resInd <- rbind(resInd, 
                          cbind.data.frame(xcor2 = centroidTerr[,"xcor"], ycor2 = centroidTerr[,"ycor"],
                                           who = eachMale))
        }
      }
    }
  }
  
  # At the end of the simulation run, merge the lynx born territory centroid 
  # with the centroid of the place where they became residents based on their ID
  # and calculate the distance
  if(nrow(bornInd) != 0){
    
    bornResInd <- merge(bornInd, resInd, by = "who", all = TRUE)
    bornResInd <- na.omit(bornResInd)
    if(nrow(bornResInd) != 0){
      bornResInd$dist <- spDists(x = spTransform(SpatialPoints(coords = cbind(pxcor = bornResInd$xcor, pycor = bornResInd$ycor),
                                                               proj4string = habMapSpaDES@crs),
                                                 "+proj=longlat +ellps=WGS84"),
                                 y = spTransform(SpatialPoints(coords = cbind(pxcor = bornResInd$xcor2, pycor = bornResInd$ycor2),
                                                               proj4string = habMapSpaDES@crs),
                                                 "+proj=longlat +ellps=WGS84"),
                                 diagonal = TRUE)
      dispDist <- c(dispDist, bornResInd$dist[!is.na(bornResInd$dist)])
    }
  }

  print(i)
}
  
  
  dispDistList[[popList]] <- dispDist
  popList <- popList + 1
}  

summary(dispDistList[[1]]) # Alps
summary(dispDistList[[2]]) # Jura
summary(dispDistList[[3]]) # Vosges-Palatinate
summary(dispDistList[[4]]) # BlackForest


########################
## Dispersal duration ##
########################
dispTime <- c()

for(i in 1:length(listSim)){ # for each simulation run
  load(paste0(pathFiles, "/", listSim[i]))
  
  # Which day the dispersers became residents
  timeRes <- lynxIBMrun$timeRes
  timeRes <- timeRes[timeRes$year >= 10, ] # remove the burn-in
  dispTime <- c(dispTime, timeRes$time)
  # Add 365 for the individuals still dispersers at the end of the year
  for(y in 10:(lastYear - 1)){ 
    disp <- NLwith(agents = lynxIBMrun$outputLynx[[y]], var = "status", val = "disp")
    notDispOfTheYear <- other(agents = disp, except = NLwith(agents = disp, var = "age", val = 2))
    nDisp <- NLcount(notDispOfTheYear)
    dispTime <- c(dispTime, rep(365, nDisp))
  }
  
  print(i)
}

summary(dispTime)


##############################
## Daily dispersal distance ##
##############################
dailyDist <- c()

for(i in 1:length(listSim)){ # for each simulation run
  load(paste0(pathFiles, "/", listSim[i]))
  
  # Compile the daily dispersal distances
  dailyDistSim <- lynxIBMrun$dailyDist
  dailyDistSim <- dailyDistSim[dailyDistSim$year >= 10, "dailyDist"] # remove the burn-in
  dailyDist <- c(dailyDist, dailyDistSim)
  
  print(i)
}

summary(dailyDist)


#######################
## Habitat selection ##
#######################
occHab <- c()

for(i in 1:length(listSim)){ # for each simulation run
  load(paste0(pathFiles, "/", listSim[i]))
  
  # Which habitat have been selected in the model
  occHabSim <- lynxIBMrun$occHab
  occHabSim <- occHabSim[occHabSim$year >= 10, "occHab"] # remove the burn-in
  occHab <- c(occHab, occHabSim)
  
  print(i)
}

occHab[occHab == 4] <- 3 # consider 4 (breeding habitat) and 3 (dispersal habitat) similar
table(occHab) / length(occHab)


