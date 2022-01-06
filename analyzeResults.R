library(NetLogoR)
library(colorspace)
library(ggplot2)
library(data.table)
library(Rmisc)
library(raster)
library(mapview)

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
  resLynx <- NLwith(agents = lynxIBMrun$outputLynx[[lastYear]], var = "status", val = "res") # residents lynx
  resAdults <- NLwith(agents = resLynx, var = "age", val = 1:20) # adult are between 1 year old (2nd year of life, not kitten of the year) and 20 years (oldest age possible)
  patchesLynx <- patchHere(world = lynxIBMrun$habitatMap, turtles = resAdults) # retrieve the occupied cells
  patchesLynxCnt <- ddply(as.data.frame(patchesLynx),. (pxcor, pycor), nrow)
  mapLynxAbund <- NLset(world = mapLynxAbund, agents = cbind(pxcor = patchesLynxCnt$pxcor, pycor = patchesLynxCnt$pycor),
                        val = of(world = mapLynxAbund, agents = cbind(pxcor = patchesLynxCnt$pxcor, pycor = patchesLynxCnt$pycor)) + patchesLynxCnt$V1)
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
movePopLong <- melt(as.data.frame(movePop), id = c("repSim", "year"))
movePopLongDT <- data.table(movePopLong)
# Calculate the cumulative sum of the movement over the years per simulation and per variable (=pair of populations with a direction)
movePopLongDT[, Cum.Sum := cumsum(value), by=list(repSim, variable)] 
# Calculte the mean and 95% confidence intervals per year and per variable
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
plot(pays, add = TRUE)

