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
library(terra)
library(dplyr)
library(sf)

#############################
## Analyze the simulations ##
#############################
# Read all simulated files
pathFiles <- "C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/outputs"
listSim <- list.files(pathFiles)[3:102] # remove the first two files (cache and bestCalibration)
nSim <- length(listSim)
lastYear <- 50

################################################################
#######################
## MODEL CALIBRATION ##
#######################

habMapSpaDES <- rast("C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/inputs/habMap.tif")

# Mortality
rAllRes_Alps = numeric() 
rAllDisp_Alps = numeric()
rCollRes_Alps = numeric()
rCollDisp_Alps = numeric()
nCollAlps = numeric()
# Mortality Jura
rAllRes_Jura = numeric()
rAllDisp_Jura = numeric()
rCollRes_Jura = numeric()
rCollDisp_Jura = numeric()
nCollJura = numeric()
# Reproduction
pRepro = numeric()
# Dispersal distance
distDispAlps = numeric()
distDispJura = numeric()

for(file in 1:length(listSim)){
  load(paste0(pathFiles, "/", listSim[file]))
  
  ############
  ## Mortality
  
  for(y in 1:(lastYear+1)){
    
    # The population in which lynx died is updated once they died
    if(NLcount(lynxIBMrun$deadLynxColl[[y]]) == 0){
      deadLynxColl <- noTurtles()
    } else {
      deadLynxColl <- lynxIBMrun$deadLynxColl[[y]]
    }
    if(NLcount(lynxIBMrun$deadLynxNoColl[[y]] & NLcount(lynxIBMrun$deadOldLynx[[y]])) == 0){ # add the old lynx
      deadLynxNoColl <- noTurtles()
    } else {
      deadLynxNoColl <- turtleSet(lynxIBMrun$deadLynxNoColl[[y]], lynxIBMrun$deadOldLynx[[y]])
    }
    # However, the population in which lynx are at the beginning of the time step is not updated
    # Their "pop" is the population in which they are born
    # Need to assign Alps and Jura population based on the lynx current location
    # Also some lynx were in a population at the beginning of the time step and they died in another population
    # To avoid case where this happen, we need to update the population for those to be part, at the beginning
    # of the time step, in the population where they died so that individuals dead are in the fraction,
    # same as for their status (for example, if a lynx started the year as a disperser but died as a resident,
    # it needs to be counted as a resident at the beginning of the time step)
    whoDead <-  c(of(agents = deadLynxColl, var = "who"), of(agents = deadLynxNoColl, var = "who"))
    allDead <- turtleSet(deadLynxNoColl, deadLynxColl)
    if(NLcount(allDead) != 0){
      allDeadInfo <- of(agents = allDead, var = c("who", "status", "pop"))
      allDeadInfo <- allDeadInfo[order(allDeadInfo$who),]
      if(NLcount(lynxIBMrun$outputLynx[[y]]) == 0){
        outputLynx <- noTurtles()
      } else {
        outputLynx <- lynxIBMrun$outputLynx[[y]]
        # Update their location
        popHere <- of(world = lynxIBMrun$popDist, agents = patchHere(world = lynxIBMrun$popDist, turtles = outputLynx))
        popHereName <- rep("Unknown", length(popHere))
        popHereName[popHere == 1] <-  "Alps"
        popHereName[popHere == 2] <-  "Jura"
        popHereName[popHere == 3] <-  "Vosges-Palatinate"
        popHereName[popHere == 4] <-  "BlackForest"
        outputLynx <- NLset(turtles = outputLynx, agents = outputLynx, var = "pop", val = popHereName)
        # Update the dead ones in the outputLynx
        outputLynx <- NLset(turtles = outputLynx, agents = NLwith(agents = outputLynx, var = "who", val = whoDead),
                            var = "status", val = allDeadInfo$status)
        outputLynx <- NLset(turtles = outputLynx, agents = NLwith(agents = outputLynx, var = "who", val = whoDead),
                            var = "pop", val = allDeadInfo$pop)
      }
    } else {
      if(NLcount(lynxIBMrun$outputLynx[[y]]) == 0){
        outputLynx <- noTurtles()
      } else {
        outputLynx <- lynxIBMrun$outputLynx[[y]]
        # Update their location
        popHere <- of(world = lynxIBMrun$popDist, agents = patchHere(world = lynxIBMrun$popDist, turtles = outputLynx))
        popHereName <- rep("Unknown", length(popHere))
        popHereName[popHere == 1] <-  "Alps"
        popHereName[popHere == 2] <-  "Jura"
        popHereName[popHere == 3] <-  "Vosges-Palatinate"
        popHereName[popHere == 4] <-  "BlackForest"
        outputLynx <- NLset(turtles = outputLynx, agents = outputLynx, var = "pop", val = popHereName)
      }
    }
    
    for(popName in c("Alps", "Jura")){
      
      # The population in which lynx died is updated once they died
      if(NLcount(deadLynxColl) != 0){
        deadLynxCollPop <- NLwith(agents = deadLynxColl, var = "pop", val = popName)
      } else {
        deadLynxCollPop <- noTurtles()
      }
      if(NLcount(deadLynxNoColl) != 0){ 
        deadLynxNoCollPop <- NLwith(agents = deadLynxNoColl, var = "pop", val = popName)
      } else {
        deadLynxNoCollPop <- noTurtles()
      }
      if(NLcount(outputLynx) != 0){
        outputLynxPop <- NLwith(agents = outputLynx, var = "pop", val = popName)
      } else {
        outputLynxPop <- noTurtles()
      }
      
      # Collisions - All
      nCollAll <- NLcount(agents = deadLynxCollPop)
      
      # Collisions
      if(NLcount(deadLynxCollPop) == 0){
        nCollRes <- 0
        nCollDisp <- 0
      } else {
        nCollRes <- NLcount(agents = NLwith(agents = deadLynxCollPop, var = "status", val = "res"))
        nCollDisp <- NLcount(agents = NLwith(agents = deadLynxCollPop, var = "status", val = "disp"))
      }
      
      # Total deaths
      allDead <- turtleSet(deadLynxNoCollPop, deadLynxCollPop)
      if(NLcount(allDead) == 0){
        nDeadRes <- 0
        nDeadDisp <- 0
      } else {
        nDeadRes <- NLcount(agents = NLwith(agents = allDead, var = "status", val = "res"))
        nDeadDisp <- NLcount(agents = NLwith(agents = allDead, var = "status", val = "disp"))
      }
      
      # Number of individuals at the beginning of the yearly time step
      if(NLcount(outputLynxPop) != 0){
        nAll <- NLcount(agents = outputLynxPop)
        nRes <- NLcount(agents = NLwith(agents = outputLynxPop,
                                        var = "status", val = "res"))
        nDisp <- NLcount(agents = NLwith(agents = outputLynxPop,
                                         var = "status", val = "disp"))
      } else {
        nAll <- 0
        nRes <- 0
        nDisp <- 0
      }
      
      # Mortality rates
      if(popName == "Alps"){
        rAllRes_Alps = c(rAllRes_Alps, nDeadRes / nRes) 
        rAllDisp_Alps = c(rAllDisp_Alps, nDeadDisp / nDisp)
        rCollRes_Alps = c(rCollRes_Alps, nCollRes / nRes)
        rCollDisp_Alps = c(rCollDisp_Alps, nCollDisp / nDisp)
        nCollAlps = c(nCollAlps, nCollAll)
      }
      if(popName == "Jura"){
        rAllRes_Jura = c(rAllRes_Jura, nDeadRes / nRes)
        rAllDisp_Jura = c(rAllDisp_Jura, nDeadDisp / nDisp)
        rCollRes_Jura = c(rCollRes_Jura, nCollRes / nRes)
        rCollDisp_Jura = c(rCollDisp_Jura, nCollDisp / nDisp)
        nCollJura = c(nCollJura, nCollAll)
      }
      
    }
  }
  
  ###############
  ## Reproduction
  
  FemRes <- lynxIBMrun$FemRes # resident individuals
  nKittyBorn <- lynxIBMrun$nKittyBorn # number of kittens produced per reproducing female
  
  nFemRes <- rep(0, length(FemRes)-1) 
  nKitty <- rep(0, length(FemRes)-1)
  
  for(j in 1:(length(FemRes) - 1)){
    # Number of resident females
    nFemRes[j] <- NLcount(FemRes[[j]])
  }
  
  for(j in 1:length(nKittyBorn)){
    # Number of females which actually produced newborns
    nKitty[j] <- length(which(nKittyBorn[[j]] != 0))
  }
  
  # Reproduction rate
  pRepro <- c(pRepro, nKitty / nFemRes)
  
  
  ######################
  ## Dispersal distances
  
  for(popName in c("Alps", "Jura")){
    
    resInd <- NULL
    
    for(y in 1:(lastYear - 1)){ 
      
      # Find territories where lynx were born
      if(NLcount(agents = NLwith(agents = lynxIBMrun$bornLynx[[y]], var = "pop", val = popName)) > 0){
        
        # Current year territory centroids
        values(habMapSpaDES) <- of(world = lynxIBMrun$outputTerrMap[[y]], agents = NetLogoR::patches(lynxIBMrun$outputTerrMap[[y]])) # transfer the territory number from the worldMatrix to a raster
        terrPol <- as.polygons(habMapSpaDES, dissolve = TRUE)
        centroidTerr <- terrPol %>% 
          st_as_sf() %>% 
          st_centroid()
        # Born individuals this year
        terrBornInd <- of(world = lynxIBMrun$outputTerrMap[[y]],
                          agents = patchHere(world = lynxIBMrun$outputTerrMap[[y]], 
                                             turtles = NLwith(agents=lynxIBMrun$bornLynx[[y]],var = "pop",val = popName)))
        whoBornInd <- of(agents = NLwith(agents=lynxIBMrun$bornLynx[[y]],var = "pop",val = popName), var = "who")
        terrBornInd2 <- centroidTerr %>% 
          merge(., cbind.data.frame(habMap = terrBornInd, who = whoBornInd)) %>% 
          select(who)
        
        if(y == 1){
          bornInd <- terrBornInd2
        } else{
          bornInd <- rbind(bornInd, terrBornInd2)
        }
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
          values(habMapSpaDES) <- of(world = lynxIBMrun$outputTerrMap[[y + 1]], agents = NetLogoR::patches(lynxIBMrun$outputTerrMap[[y + 1]])) # transfer the territory number from the worldMatrix to a raster
          terrPol <- as.polygons(habMapSpaDES, dissolve = TRUE)
          centroidTerr <- terrPol %>% 
            st_as_sf() %>% 
            st_centroid()
          
          # Resident females
          terrResFem <- of(world = lynxIBMrun$outputTerrMap[[y + 1]],
                           agents = patchHere(world = lynxIBMrun$outputTerrMap[[y + 1]], turtles = resFem))
          whoResFem <- of(agents = resFem, var = "who")
          
          terrResFem2 <- centroidTerr %>% 
            merge(., cbind.data.frame(habMap = terrResFem, who = whoResFem)) %>% 
            select(who)
          
          if(y == 1){
            resInd <- terrResFem2
          } else{
            resInd <- rbind(resInd, terrResFem2)
          }
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
            terrVal <- of(world = lynxIBMrun$outputTerrMap[[y + 1]], agents = NetLogoR::patches(lynxIBMrun$outputTerrMap[[y + 1]])) # transfer the territory number from the worldMatrix to a raster
            terrVal[!terrVal %in% of(agents = allFem, var = "who")] <- NA
            values(habMapSpaDES) <- terrVal
            
            terrPol <- as.polygons(habMapSpaDES, dissolve = TRUE)
            centroidTerr <- terrPol %>% 
              st_as_sf() %>% 
              st_union %>% 
              st_centroid() %>% 
              st_sf %>% 
              mutate(who = eachMale)
            
            if(is.null(resInd)){
              resInd <- centroidTerr
            } else{
              resInd <- rbind(resInd, centroidTerr)
            }
            
          }
        }
      }
    }
    
    # Calculate the distances between the born and the resident territories based on the ID
    # Remove duplicated individuals (if they had multiple territories)
    resInd <- resInd[!duplicated(resInd$who), ]
    # Find matching who for born and residential individuals
    whoInter <- intersect(st_drop_geometry(bornInd)[,1], st_drop_geometry(resInd)[,1])
    bornIndInter <- bornInd %>% 
      filter(who %in% whoInter)
    # Bug sometimes, individuals with the same ID have different born territories
    # Remove all these individuals. Problem fixed in the model
    bornIndInter <- bornIndInter[!bornIndInter$who %in% as.numeric(names(table(bornIndInter$who)[table(bornIndInter$who) > 1])), ]
    resIndInter <- resInd %>% 
      filter(who %in% st_drop_geometry(bornIndInter)[,1]) %>% 
      arrange(who, st_drop_geometry(bornIndInter)[,1])
    distTer <- st_distance(bornIndInter, resIndInter, by_element = TRUE, which = "Euclidean")
    
    if(popName == "Alps"){
      distDispAlps <- c(distDispAlps, distTer)
    }
    if(popName == "Jura"){
      distDispJura <- c(distDispJura, distTer)
    }
  }
  
  print(file)
}

save(rAllRes_Alps, rAllDisp_Alps, rCollRes_Alps, rCollDisp_Alps, nCollAlps,
     rAllRes_Jura, rAllDisp_Jura, rCollRes_Jura, rCollDisp_Jura, nCollJura,
     pRepro, distDispAlps, distDispJura, 
     file = "C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/outputs_bestCal.RData")

# Mortality rates
# All mortalities
mortRateAll <- rbind(cbind.data.frame(mortR = rAllRes_Alps, Populations = "Alps", Status = "Residents"),
                     cbind.data.frame(mortR = rAllDisp_Alps, Populations = "Alps", Status = "Dispersers"),
                     cbind.data.frame(mortR = rAllRes_Jura, Populations = "Jura", Status = "Residents"),
                     cbind.data.frame(mortR = rAllDisp_Jura, Populations = "Jura", Status = "Dispersers"))
ggplot(data = mortRateAll, group = interaction("Populations", "Status")) +
  geom_boxplot(aes(x = Status, y = mortR, fill = Populations), outlier.shape = NA) +
  # Mean reference values
  geom_point(data = cbind.data.frame(mortR = c(0.24, 0.24, 0.17, 0.35), Populations = c("Alps", "Alps", "Jura", "Jura"), 
                                     Status = c("Residents", "Dispersers", "Residents", "Dispersers")),
             mapping = aes(x = Status, y = mortR, group = Populations), position = position_dodge(width = 0.75),
             shape = 20, size = 3) +
  # CI reference values
  geom_point(data = cbind.data.frame(mortR = c(0.08, 0.57, 0.15, 0.38, 0.1, 0.29, 0.16, 0.65), 
                                     Populations = c("Alps", "Alps", "Alps", "Alps", "Jura", "Jura", "Jura", "Jura"), 
                                     Status = c("Residents", "Residents", "Dispersers", "Dispersers", "Residents", "Residents", "Dispersers", "Dispersers")),
             mapping = aes(x = Status, y = mortR, group = Populations), position = position_dodge(width = 0.75),
             shape = 20, size = 5) +
  scale_y_continuous(limits = c(0.05, 0.65)) +
  labs(y = "Mortality rate", x = "")
  
# Collision mortalities
mortRateColl <- rbind(cbind.data.frame(mortR = rCollRes_Alps, Populations = "Alps", Status = "Residents"),
                     cbind.data.frame(mortR = rCollDisp_Alps, Populations = "Alps", Status = "Dispersers"),
                     cbind.data.frame(mortR = rCollRes_Jura, Populations = "Jura", Status = "Residents"),
                     cbind.data.frame(mortR = rCollDisp_Jura, Populations = "Jura", Status = "Dispersers"))
ggplot(data = mortRateColl, group = interaction("Populations", "Status")) +
  geom_boxplot(aes(x = Status, y = mortR, fill = Populations, color = Populations), outlier.shape = NA) +
  scale_color_manual(values = c("#F8766D", "black")) +
  # Mean reference values
  geom_point(data = cbind.data.frame(mortR = c(0.02, 0.077, 0.034, 0.067), Populations = c("Alps", "Alps", "Jura", "Jura"), 
                                     Status = c("Residents", "Dispersers", "Residents", "Dispersers")),
             mapping = aes(x = Status, y = mortR, group = Populations), position = position_dodge(width = 0.75),
             shape = 20, size = 3) +
  # CI reference values
  geom_point(data = cbind.data.frame(mortR = c(0, 0.059, 0, 0.211, 0, 0.08, 0, 0.185), 
                                     Populations = c("Alps", "Alps", "Alps", "Alps", "Jura", "Jura", "Jura", "Jura"), 
                                     Status = c("Residents", "Residents", "Dispersers", "Dispersers", "Residents", "Residents", "Dispersers", "Dispersers")),
             mapping = aes(x = Status, y = mortR, group = Populations), position = position_dodge(width = 0.75),
             shape = 20, size = 5) +
  scale_y_continuous(limits = c(0, 0.25)) +
  labs(y = "Mortality rate", x = "")

# Number of collisions
collPerYear <- rbind(cbind.data.frame(nColl = nCollAlps[seq(from = 2, to = (51 * (nSim - 1)) + 2, by = 51)], 
                                      Populations = "Alps", Year = 1),
                     cbind.data.frame(nColl = nCollAlps[seq(from = 3, to = (51 * (nSim - 1)) + 3, by = 51)], 
                                      Populations = "Alps", Year = 2),
                     cbind.data.frame(nColl = nCollAlps[seq(from = 4, to = (51 * (nSim - 1)) + 4, by = 51)], 
                                      Populations = "Alps", Year = 3),
                     cbind.data.frame(nColl = nCollAlps[seq(from = 5, to = (51 * (nSim - 1)) + 5, by = 51)],
                                      Populations = "Alps", Year = 4),
                     cbind.data.frame(nColl = nCollAlps[seq(from = 6, to = (51 * (nSim - 1)) + 6, by = 51)],
                                      Populations = "Alps", Year = 5),
                     cbind.data.frame(nColl = nCollJura[seq(from = 2, to = (51 * (nSim - 1)) + 2, by = 51)],
                                      Populations = "Jura", Year = 1),
                     cbind.data.frame(nColl = nCollJura[seq(from = 3, to = (51 * (nSim - 1)) + 3, by = 51)],
                                      Populations = "Jura", Year = 2),
                     cbind.data.frame(nColl = nCollJura[seq(from = 4, to = (51 * (nSim - 1)) + 4, by = 51)], 
                                      Populations = "Jura", Year = 3),
                     cbind.data.frame(nColl = nCollJura[seq(from = 5, to = (51 * (nSim - 1)) + 5, by = 51)], 
                                      Populations = "Jura", Year = 4),
                     cbind.data.frame(nColl = nCollJura[seq(from = 6, to = (51 * (nSim - 1)) + 6, by = 51)], 
                                      Populations = "Jura", Year = 5)
                     )
# Collisions Alps
ggplot(data = collPerYear[collPerYear$Populations == "Alps",]) +
  geom_boxplot(aes(x = Year, y = nColl, group = Year), fill = "#F8766D", color = "#F8766D") +
  geom_point(data = cbind.data.frame(nColl = c(2, 0, 3, 1, 3), Populations = "Alps", 
                                     Year = 1:5),
             mapping = aes(x = Year, y = nColl, group = Year), position = position_dodge(width = 0.75),
             shape = 20, size = 3) +
  scale_y_continuous(limits = c(0, 4)) +
  labs(y = "Number of collisions")
# Collisions Jura
ggplot(data = collPerYear[collPerYear$Populations == "Jura",]) +
  geom_boxplot(aes(x = Year, y = nColl, group = Year), fill = "#00BFC4", outlier.shape = NA) +
  #geom_boxplot(aes(x = Year, y = nColl, group = Year), color = "#00BFC4") +
  geom_point(data = cbind.data.frame(nColl = c(6, 10, 4, 5, 5), Populations = "Jura", 
                                     Year = 1:5),
             mapping = aes(x = Year, y = nColl, group = Year), position = position_dodge(width = 0.75),
             shape = 20, size = 3) +
  scale_y_continuous(limits = c(0, 12)) +
  labs(y = "Number of collisions")
  
# Reproduction
ggplot(data = as.data.frame(pRepro)) +
  geom_boxplot(aes(x = "",  y = pRepro), outlier.shape = NA) +
  geom_point(data = data.frame(pRepro = 0.81),
             mapping = aes(x = "", y = pRepro), position = position_dodge(width = 0.75),
             shape = 20, size = 3) +
  scale_y_continuous(limits = c(0.7, 0.95))

# Distances
distDisp <- rbind(cbind.data.frame(distD = distDispAlps / 1000, Populations = "Alps"),
                      cbind.data.frame(distD = distDispJura / 1000, Populations = "Jura"))
ggplot(data = distDisp, fill = Populations) +
  geom_boxplot(aes(x = "", y = distD, fill = Populations), outlier.shape = NA) +
  # Mean reference values
  geom_point(data = cbind.data.frame(distD = c(26, 63), Populations = c("Alps", "Jura")),
             mapping = aes(x = "", y = distD, group = Populations), position = position_dodge(width = 0.75),
             shape = 20, size = 3) +
  # CI reference values
  geom_point(data = cbind.data.frame(distD = c(15, 36, 39, 87), 
                                     Populations = c("Alps", "Alps", "Jura", "Jura")),
             mapping = aes(x = "", y = distD, group = Populations), position = position_dodge(width = 0.75),
             shape = 20, size = 5) +
  scale_y_continuous(limits = c(0, 150)) +
  labs(y = "Dispersal distance (km)", x = "")


################################################################


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
                         agents = NLwith(agents = NetLogoR::patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 1))
    popAlps[popAlps[, "year"] == y & popAlps[, "repSim"] == i, "nInd"] <- NLcount(indAlps) 
    
    indJura <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$outputLynx[[y]],
                         agents = NLwith(agents = NetLogoR::patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 2))
    popJura[popJura[, "year"] == y & popJura[, "repSim"] == i, "nInd"] <- NLcount(indJura) 
   
    indVosPal <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$outputLynx[[y]],
                           agents = NLwith(agents = NetLogoR::patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 3))
    popVP[popVP[, "year"] == y & popVP[, "repSim"] == i, "nInd"] <- NLcount(indVosPal) 
        
    indBF <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$outputLynx[[y]],
                         agents = NLwith(agents = NetLogoR::patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 4))
    popBF[popBF[, "year"] == y & popBF[, "repSim"] == i, "nInd"] <- NLcount(indBF)
  }
  print(i)
}

# Reload to be sure the saved results are the original ones
load("C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/outputs_bestCal.RData")
save(rAllRes_Alps, rAllDisp_Alps, rCollRes_Alps, rCollDisp_Alps, nCollAlps,
     rAllRes_Jura, rAllDisp_Jura, rCollRes_Jura, rCollDisp_Jura, nCollJura,
     pRepro, distDispAlps, distDispJura, 
     popAlps, popJura, popVP, popBF,
     file = "C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/outputs_bestCal.RData")


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
  # Growth rate = 1
  geom_line(data = cbind.data.frame(year = 1:51, GR = 1), 
            mapping = aes(x = year, y = GR), color = "grey10", linetype = "dashed") +
  coord_cartesian(ylim=c(0.9, 1.5)) +
  annotate("rect", xmin = -Inf, xmax = 3, ymin = -Inf, ymax = Inf, alpha = .7) +
  labs(x ="Years simulated", y = "Population growth rates")


##################
## Lynx density ##
##################
# Load a map from any simulation to have the study area
load(paste0(pathFiles, "/", listSim[1]))
mapLynxAbund <- NLset(world = lynxIBMrun$habitatMap, agents = NetLogoR::patches(lynxIBMrun$habitatMap), val = 0) # empty a map

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
mapLynxAbundRas <- raster("C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/inputs/habMap.tif")
# Rescale the value to obtain a mean over all simulations
mapLynxAbundRas[] <- of(world = mapLynxAbund, agents = NetLogoR::patches(mapLynxAbund)) / nSim
# The map before was of 1 km2, transform it into 100 km2
mapLynxAbund100km2 <- aggregate(mapLynxAbundRas, fact = 10, fun = sum)
# Give NA where there were no lynx simulated
mapLynxAbund100km2[mapLynxAbund100km2 == 0] <- NA
# Plot the density with the country borders
plot(mapLynxAbund100km2, col = wes_palette("Zissou1", 100, type = "continuous"))
pays <- shapefile("C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/inputs/countryBorders.shp")
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
                         agents = NLwith(agents = NetLogoR::patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 1))
    resJura <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$resLynx[[y]],
                         agents = NLwith(agents = NetLogoR::patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 2))
    resVP <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$resLynx[[y]],
                       agents = NLwith(agents = NetLogoR::patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 3))
    resBF <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$resLynx[[y]],
                       agents = NLwith(agents = NetLogoR::patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 4))
    
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

load("C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/outputs_bestCal.RData")
save(rAllRes_Alps, rAllDisp_Alps, rCollRes_Alps, rCollDisp_Alps, nCollAlps,
     rAllRes_Jura, rAllDisp_Jura, rCollRes_Jura, rCollDisp_Jura, nCollJura,
     pRepro, distDispAlps, distDispJura, 
     popAlps, popJura, popVP, popBF,
     movePop,
     file = "C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/outputs_bestCal.RData")

# Non-cumulative number of individuals
library(Rmisc)
library(dplyr)

movePopLongDT <- melt(setDT(as.data.frame(movePop)), id = c("repSim", "year"))
movePopDF <- as.data.frame(movePopLongDT) 
meanMovePop <- movePopDF %>% group_by(year, variable) %>% 
  summarize(mean = mean(value),
            lower = CI(value)[3],
            upper = CI(value)[1])
meanMovePop[meanMovePop$lower < 0, "lower"] <- 0
# Rename the variable to display in the legend of the plot
meanMovePop <- as.data.frame(meanMovePop)
colnames(meanMovePop)[2] <- "Populations"

# Plot
colRainbow <- rainbow_hcl(n = 12, c = 130, l = 70)

ggplot(meanMovePop, aes(x = year, y = mean, colour = Populations)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, x = year, fill = Populations), alpha = 0.3) +
  geom_line() +
  geom_point() +
  labs(x= "Years simulated", y = "Number of individuals establishing outside of their native population") +
  scale_color_manual(values = colRainbow, name = "", labels = c("Alps to Jura", "Alps to Black Forest", "Alps to Vosges-Palatinate", "Jura to Alps",
                                                     "Jura to Black Forest", "Jura to Vosges-Palatinate", "Black Forest to Alps",
                                                     "Black Forest to Jura", "Black Forest to Vosges-Palatinate", "Vosges-Palatinate to Alps",
                                                     "Vosges-Palatinate to Black Forest", "Vosges-Palatinate to Jura")) +
  scale_fill_manual(values = colRainbow, name = "", labels = c("Alps to Jura", "Alps to Black Forest", "Alps to Vosges-Palatinate", "Jura to Alps",
                                                                "Jura to Black Forest", "Jura to Vosges-Palatinate", "Black Forest to Alps",
                                                                "Black Forest to Jura", "Black Forest to Vosges-Palatinate", "Vosges-Palatinate to Alps",
                                                                "Vosges-Palatinate to Black Forest", "Vosges-Palatinate to Jura")) +
  annotate("rect", xmin = -Inf, xmax = 3, ymin = -Inf, ymax = Inf, alpha = .7)

# In four different plot for the different populations
ggplot(meanMovePop[meanMovePop$Populations %in% c("AtoJ", "AtoVP", "AtoBF"), ], aes(x = year, y = mean, colour = Populations)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, x = year, fill = Populations), alpha = 0.3) +
  geom_line() +
  geom_point() +
  #labs(x= "Years simulated", y = "Number of individuals establishing outside of their native population") +
  labs(x= "Years simulated", y = "Number of individuals") +
  # scale_color_manual(values = c("#00BFC4",  "#C77CFF", "#7CAE00"), name = "", labels = c("Alps to Jura", "Alps to Black Forest", "Alps to Vosges-Palatinate")) +
  # scale_fill_manual(values = c("#00BFC4", "#C77CFF", "#7CAE00"), name = "", labels = c("Alps to Jura", "Alps to Black Forest", "Alps to Vosges-Palatinate")) +
  scale_color_manual(values = c("#00BFC4",  "#C77CFF", "#7CAE00"), name = "", labels = c("In Jura", "In Black Forest", "In Vosges-Palatinate")) +
  scale_fill_manual(values = c("#00BFC4", "#C77CFF", "#7CAE00"), name = "", labels = c("In Jura", "In Black Forest", "In Vosges-Palatinate")) +
  annotate("rect", xmin = -Inf, xmax = 3, ymin = -Inf, ymax = Inf, alpha = .7) +
  theme(legend.position = "bottom")

ggplot(meanMovePop[meanMovePop$Populations %in% c("JtoA", "JtoVP", "JtoBF"), ], aes(x = year, y = mean, colour = Populations)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, x = year, fill = Populations), alpha = 0.3) +
  geom_line() +
  geom_point() +
  #labs(x= "Years simulated", y = "Number of individuals establishing outside of their native population") +
  labs(x= "Years simulated", y = "Number of individuals") +
  # scale_color_manual(values = c("#F8766D",  "#C77CFF", "#7CAE00"), name = "", labels = c("Jura to Alps", "Jura to Black Forest", "Jura to Vosges-Palatinate")) +
  # scale_fill_manual(values = c("#F8766D",  "#C77CFF", "#7CAE00"), name = "", labels = c("Jura to Alps", "Jura to Black Forest", "Jura to Vosges-Palatinate")) +
  scale_color_manual(values = c("#F8766D",  "#C77CFF", "#7CAE00"), name = "", labels = c("In Alps", "In Black Forest", "In Vosges-Palatinate")) +
  scale_fill_manual(values = c("#F8766D",  "#C77CFF", "#7CAE00"), name = "", labels = c("In Alps", "In Black Forest", "In Vosges-Palatinate")) +
  annotate("rect", xmin = -Inf, xmax = 3, ymin = -Inf, ymax = Inf, alpha = .7) +
  theme(legend.position = "bottom")

ggplot(meanMovePop[meanMovePop$Populations %in% c("VPtoA", "VPtoJ", "VPtoBF"), ], aes(x = year, y = mean, colour = Populations)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, x = year, fill = Populations), alpha = 0.3) +
  geom_line() +
  geom_point() +
  #labs(x= "Years simulated", y = "Number of individuals establishing outside of their native population") +
  labs(x= "Years simulated", y = "Number of individuals") +
  # scale_color_manual(values = c("#F8766D",  "#00BFC4", "#C77CFF"), name = "", labels = c("Vosges-Palatinate to Alps", "Vosges-Palatinate to Jura", "Vosges-Palatinate to Black Forest")) +
  # scale_fill_manual(values = c("#F8766D",  "#00BFC4", "#C77CFF"), name = "", labels = c("Vosges-Palatinate to Alps", "Vosges-Palatinate to Jura", "Vosges-Palatinate to Black Forest")) +
  scale_color_manual(values = c("#F8766D",  "#00BFC4", "#C77CFF"), name = "", labels = c("In Alps", "In Jura", "In Black Forest")) +
  scale_fill_manual(values = c("#F8766D",  "#00BFC4", "#C77CFF"), name = "", labels = c("In Alps", "In Jura", "In Black Forest")) +
  annotate("rect", xmin = -Inf, xmax = 3, ymin = -Inf, ymax = Inf, alpha = .7) +
  theme(legend.position = "bottom")

ggplot(meanMovePop[meanMovePop$Populations %in% c("BFtoA", "BFtoJ", "BFtoVP"), ], aes(x = year, y = mean, colour = Populations)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, x = year, fill = Populations), alpha = 0.3) +
  geom_line() +
  geom_point() +
  #labs(x= "Years simulated", y = "Number of individuals establishing outside of their native population") +
  labs(x= "Years simulated", y = "Number of individuals") +
  # scale_color_manual(values = c("#F8766D",  "#00BFC4", "#7CAE00"), name = "", labels = c("Black Forest to Alps", "Black Forest to Jura", "Black Forest to Vosges-Palatinate")) +
  # scale_fill_manual(values = c("#F8766D",  "#00BFC4", "#7CAE00"), name = "", labels = c("Black Forest to Alps", "Black Forest to Jura", "Black Forest to Vosges-Palatinate")) +
  scale_color_manual(values = c("#F8766D",  "#00BFC4", "#7CAE00"), name = "", labels = c("In Alps", "In Jura", "In Vosges-Palatinate")) +
  scale_fill_manual(values = c("#F8766D",  "#00BFC4", "#7CAE00"), name = "", labels = c("In Alps", "In Jura", "In Vosges-Palatinate")) +
  annotate("rect", xmin = -Inf, xmax = 3, ymin = -Inf, ymax = Inf, alpha = .7) +
  theme(legend.position = "bottom")


#########################
## Territory occupancy ##
#########################
# Load a map from any simulation to have the study area
load(paste0(pathFiles, "/", listSim[1]))
terrOccMap <- NLset(world = lynxIBMrun$outputTerrMap[[1]], agents = NetLogoR::patches(lynxIBMrun$outputTerrMap[[1]]), val = 0)  # empty a map

for(i in 1:length(listSim)){ # for each simulation run
  load(paste0(pathFiles, "/", listSim[i]))
  terrOccMapSim <- lynxIBMrun$outputTerrMap[[lastYear]]
  valTerrOccMapSim <- of(world = terrOccMapSim, agents = NetLogoR::patches(terrOccMapSim))
  # Transform the value of the territories into 0 and 1 for presence or abscence of a territory
  valTerrOccMapSim[!is.na(valTerrOccMapSim)] <- 1
  valTerrOccMapSim[is.na(valTerrOccMapSim)] <- 0
  terrOccMap <- NLset(world = terrOccMap, agents = NetLogoR::patches(terrOccMap),
                      val = of(world = terrOccMap, agents = NetLogoR::patches(terrOccMap)) + valTerrOccMapSim)
  print(i)
}

# Transfer the data from the worldMatrix map to a raster format
terrOccMapRas <- raster("C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/inputs/habMap.tif")
# Rescale the value to obtain a mean over all simulations
terrOccMapRas[] <- of(world = terrOccMap, agents = NetLogoR::patches(terrOccMap)) / nSim
# Give NA where there were no female lynx territory simulated
terrOccMapRas[terrOccMapRas == 0] <- NA
# Plot the territory occupancy with the country borders
plot(terrOccMapRas)
pays <- shapefile("C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/inputs/countryBorders.shp")
plot(pays, add = TRUE)

# Include the non-used breeding habitat
habMapSpaDES <- raster("C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/inputs/habMap.tif")
habMapSpaDES[habMapSpaDES %in% c(0, 1, 2, 3)] <- NA
habMapSpaDES[habMapSpaDES == 4] <- 1
habMapSpaDES[!is.na(terrOccMapRas)] <- NA
plot(habMapSpaDES)
habPol <- rasterToPolygons(habMapSpaDES)
# Adapt the colorscale
plot(terrOccMapRas, col = wes_palette("Zissou1", 100, type = "continuous"))
plot(habPol, col = "gray80", border = NA, add = TRUE)
plot(pays, add = TRUE)


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
    notDispOfTheYear <- other(agents = disp, except = NLwith(agents = disp, var = "age", val = c(1, 2)))
    nDisp <- NLcount(notDispOfTheYear)
    dispTime <- c(dispTime, rep(365, nDisp))
  }
  
  print(i)
}

mean(dispTime)


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


