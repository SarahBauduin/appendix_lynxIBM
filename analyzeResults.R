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
pathFiles <- "appendix_lynxIBM/module/outputs"
listSim <- list.files(pathFiles)[3:102] # remove the first two files (cache and bestCalibration)
nSim <- length(listSim)
lastYear <- 50

# Play with the color from the palet used by ggplot
gg_color <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


################################################################
#######################
## MODEL CALIBRATION ##
#######################

calibrIBM <- data.frame(## Reference values
                        # Mortality Alps
                        mean_rAllRes_Alps_ref = 0.24, CI_inf_rAllRes_Alps_ref = 0.08, CI_sup_rAllRes_Alps_ref = 0.57,
                        mean_rAllDisp_Alps_ref = 0.24, CI_inf_rAllDisp_Alps_ref = 0.15, CI_sup_rAllDisp_Alps_ref = 0.38,
                        mean_rCollRes_Alps_ref = 0.02, CI_inf_rCollRes_Alps_ref = 0, CI_sup_rCollRes_Alps_ref = 0.059,
                        mean_rCollDisp_Alps_ref = 0.077, CI_inf_rCollDisp_Alps_ref = 0, CI_sup_rCollDisp_Alps_ref = 0.211,
                        mean_nCollAlps_ref = 2, CI_inf_nCollAlps_ref = 0, CI_sup_nCollAlps_ref = 3,
                        # Mortality Jura
                        mean_rAllRes_Jura_ref = 0.17, CI_inf_rAllRes_Jura_ref = 0.1, CI_sup_rAllRes_Jura_ref = 0.29,
                        mean_rAllDisp_Jura_ref = 0.35, CI_inf_rAllDisp_Jura_ref = 0.16, CI_sup_rAllDisp_Jura_ref = 0.65,
                        mean_rCollRes_Jura_ref = 0.034, CI_inf_rCollRes_Jura_ref = 0, CI_sup_rCollRes_Jura_ref = 0.08,
                        mean_rCollDisp_Jura_ref = 0.067, CI_inf_rCollDisp_Jura_ref = 0, CI_sup_rCollDisp_Jura_ref = 0.185,
                        mean_nCollJura_ref = 6, CI_inf_nCollJura_ref = 3, CI_sup_nCollJura_ref = 9,
                        # Reproducion
                        rRepro_ref = 0.81,
                        # Dispersal distance
                        mean_distDispAlps_ref = 26, CI_inf_distDispAlps_ref = 15, CI_sup_distDispAlps_ref = 36,
                        mean_distDispJura_ref = 63, CI_inf_distDispJura_ref = 39, CI_sup_distDispJura_ref = 87,
                        
                        ## Simulated results
                        # Mortality Alps
                        mean_rAllRes_Alps = NA, CI_inf_rAllRes_Alps = NA, CI_sup_rAllRes_Alps = NA,
                        mean_rAllDisp_Alps = NA, CI_inf_rAllDisp_Alps = NA, CI_sup_rAllDisp_Alps = NA,
                        mean_rCollRes_Alps = NA, CI_inf_rCollRes_Alps = NA, CI_sup_rCollRes_Alps = NA,
                        mean_rCollDisp_Alps = NA, CI_inf_rCollDisp_Alps = NA, CI_sup_rCollDisp_Alps = NA,
                        mean_nCollAlps = NA, CI_inf_nCollAlps = NA, CI_sup_nCollAlps = NA,
                        # Mortality Jura
                        mean_rAllRes_Jura = NA, CI_inf_rAllRes_Jura = NA, CI_sup_rAllRes_Jura = NA,
                        mean_rAllDisp_Jura = NA, CI_inf_rAllDisp_Jura = NA, CI_sup_rAllDisp_Jura = NA,
                        mean_rCollRes_Jura = NA, CI_inf_rCollRes_Jura = NA, CI_sup_rCollRes_Jura = NA,
                        mean_rCollDisp_Jura = NA, CI_inf_rCollDisp_Jura = NA, CI_sup_rCollDisp_Jura = NA,
                        mean_nCollJura = NA, CI_inf_nCollJura = NA, CI_sup_nCollJura = NA,
                        # Reproducion
                        mean_rRepro = NA, CI_inf_rRepro = NA, CI_sup_rRepro = NA,
                        # Dispersal distance
                        mean_distDispAlps = NA, CI_inf_distDispAlps = NA, CI_sup_distDispAlps = NA,
                        mean_distDispJura = NA, CI_inf_distDispJura = NA, CI_sup_distDispJura = NA
)
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
  
  for(y in 3:(lastYear)){
    
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
        if(y <= 7){ # Number of collisions only for the first 5 years
          nCollAlps = c(nCollAlps, nCollAll)
        }
      }
      if(popName == "Jura"){
        rAllRes_Jura = c(rAllRes_Jura, nDeadRes / nRes)
        rAllDisp_Jura = c(rAllDisp_Jura, nDeadDisp / nDisp)
        rCollRes_Jura = c(rCollRes_Jura, nCollRes / nRes)
        rCollDisp_Jura = c(rCollDisp_Jura, nCollDisp / nDisp)
        if(y <= 7){ # Number of collisions only for the first 5 years
          nCollJura = c(nCollJura, nCollAll)
        }
      }
      
    }
  }
  
  ###############
  ## Reproduction
  
  FemRes <- lynxIBMrun$FemRes # resident individuals
  nKittyBorn <- lynxIBMrun$nKittyBorn # number of kittens produced per reproducing female
  
  nFemRes <- rep(0, length(FemRes) - 3) 
  nKitty <- rep(0, length(FemRes) - 3)
  
  for(j in 3:(length(FemRes) - 1)){ # remove burn-in
    # Number of resident females
    nFemRes[j-2] <- NLcount(FemRes[[j]])
  }
  
  for(j in 3:length(nKittyBorn)){ # remove burn-in
    # Number of females which actually produced newborns
    nKitty[j-2] <- length(which(nKittyBorn[[j]] != 0))
  }
  
  # Reproduction rate
  pRepro <- c(pRepro, nKitty / nFemRes)
  
  
  ######################
  ## Dispersal distances
  
  for(popName in c("Alps", "Jura")){
    
    resInd <- NULL
    
    for(y in 3:(lastYear - 1)){ 
      
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
        
        if(y == 3){
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
          
          if(y == 3){
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

# Summary for best calibration
# Mortality Alps
calibrIBM[1, "mean_rAllRes_Alps"] <- mean(rAllRes_Alps, na.rm = TRUE)
calibrIBM[1, "CI_inf_rAllRes_Alps"] <- t.test(rAllRes_Alps, na.rm = TRUE)$conf.int[1]
calibrIBM[1, "CI_sup_rAllRes_Alps"] <- t.test(rAllRes_Alps, na.rm = TRUE)$conf.int[2]
calibrIBM[1, "mean_rAllDisp_Alps"] <- mean(rAllDisp_Alps, na.rm = TRUE)
calibrIBM[1, "CI_inf_rAllDisp_Alps"] <- t.test(rAllDisp_Alps, na.rm = TRUE)$conf.int[1]
calibrIBM[1, "CI_sup_rAllDisp_Alps"] <- t.test(rAllDisp_Alps, na.rm = TRUE)$conf.int[2]
calibrIBM[1, "mean_rCollRes_Alps"] <- mean(rCollRes_Alps, na.rm = TRUE)
calibrIBM[1, "CI_inf_rCollRes_Alps"] <- t.test(rCollRes_Alps, na.rm = TRUE)$conf.int[1]
calibrIBM[1, "CI_sup_rCollRes_Alps"] <- t.test(rCollRes_Alps, na.rm = TRUE)$conf.int[2]
calibrIBM[1, "mean_rCollDisp_Alps"] <- mean(rCollDisp_Alps, na.rm = TRUE)
calibrIBM[1, "CI_inf_rCollDisp_Alps"] <- t.test(rCollDisp_Alps, na.rm = TRUE)$conf.int[1]
calibrIBM[1, "CI_sup_rCollDisp_Alps"] <- t.test(rCollDisp_Alps, na.rm = TRUE)$conf.int[2]
calibrIBM[1, "mean_nCollAlps"] <- mean(nCollAlps, na.rm = TRUE)
calibrIBM[1, "CI_inf_nCollAlps"] <- t.test(nCollAlps, na.rm = TRUE)$conf.int[1]
calibrIBM[1, "CI_sup_nCollAlps"] <- t.test(nCollAlps, na.rm = TRUE)$conf.int[2]
# Mortality Jura
calibrIBM[1, "mean_rAllRes_Jura"] <- mean(rAllRes_Jura, na.rm = TRUE)
calibrIBM[1, "CI_inf_rAllRes_Jura"] <- t.test(rAllRes_Jura, na.rm = TRUE)$conf.int[1]
calibrIBM[1, "CI_sup_rAllRes_Jura"] <- t.test(rAllRes_Jura, na.rm = TRUE)$conf.int[2]
calibrIBM[1, "mean_rAllDisp_Jura"] <- mean(rAllDisp_Jura, na.rm = TRUE)
calibrIBM[1, "CI_inf_rAllDisp_Jura"] <- t.test(rAllDisp_Jura, na.rm = TRUE)$conf.int[1]
calibrIBM[1, "CI_sup_rAllDisp_Jura"] <- t.test(rAllDisp_Jura, na.rm = TRUE)$conf.int[2]
calibrIBM[1, "mean_rCollRes_Jura"] <- mean(rCollRes_Jura, na.rm = TRUE)
calibrIBM[1, "CI_inf_rCollRes_Jura"] <- t.test(rCollRes_Jura, na.rm = TRUE)$conf.int[1]
calibrIBM[1, "CI_sup_rCollRes_Jura"] <- t.test(rCollRes_Jura, na.rm = TRUE)$conf.int[2]
calibrIBM[1, "mean_rCollDisp_Jura"] <- mean(rCollDisp_Jura, na.rm = TRUE)
calibrIBM[1, "CI_inf_rCollDisp_Jura"] <- t.test(rCollDisp_Jura, na.rm = TRUE)$conf.int[1]
calibrIBM[1, "CI_sup_rCollDisp_Jura"] <- t.test(rCollDisp_Jura, na.rm = TRUE)$conf.int[2]
calibrIBM[1, "mean_nCollJura"] <- mean(nCollJura, na.rm = TRUE)
calibrIBM[1, "CI_inf_nCollJura"] <- t.test(nCollJura, na.rm = TRUE)$conf.int[1]
calibrIBM[1, "CI_sup_nCollJura"] <- t.test(nCollJura, na.rm = TRUE)$conf.int[2]
# Reproduction
calibrIBM[1, "mean_rRepro"] <- mean(pRepro, na.rm = TRUE)
calibrIBM[1, "CI_inf_rRepro"] <- t.test(pRepro, na.rm = TRUE)$conf.int[1]
calibrIBM[1, "CI_sup_rRepro"] <- t.test(pRepro, na.rm = TRUE)$conf.int[2]
# Dispersal distance
calibrIBM[1, "mean_distDispAlps"] <- mean(distDispAlps, na.rm = TRUE) / 1000
calibrIBM[1, "CI_inf_distDispAlps"] <- t.test(distDispAlps, na.rm = TRUE)$conf.int[1] / 1000
calibrIBM[1, "CI_sup_distDispAlps"] <- t.test(distDispAlps, na.rm = TRUE)$conf.int[2]/ 1000
calibrIBM[1, "mean_distDispJura"] <- mean(distDispJura, na.rm = TRUE) / 1000
calibrIBM[1, "CI_inf_distDispJura"] <- t.test(distDispJura, na.rm = TRUE)$conf.int[1] / 1000
calibrIBM[1, "CI_sup_distDispJura"] <- t.test(distDispJura, na.rm = TRUE)$conf.int[2] / 1000

save(calibrIBM, file = "C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/outputs_bestCal.RData")



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
  coord_cartesian(ylim=c(0.8, 1.6)) +
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
mapLynxAbundRas <- raster("appendix_lynxIBM/module/inputs/habMap.tif")
# Rescale the value to obtain a mean over all simulations
mapLynxAbundRas[] <- of(world = mapLynxAbund, agents = NetLogoR::patches(mapLynxAbund)) / nSim
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
  annotate("rect", xmin = -Inf, xmax = 3, ymin = -Inf, ymax = Inf, alpha = .7)


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

# Plot
colRainbow <- rainbow_hcl(n = 12, c = 130, l = 70)

ggplot(meanMovePop, aes(x = year, y = mean, colour = variable)) + 
  #geom_ribbon(aes(ymin = lower, ymax = upper, x = year, fill = variable), alpha = 0.3) +
  geom_line() +
  geom_point() +
  labs(x= "Years simulated", y = "Number of individuals establishing outside of their native population", color = "Populations") +
  scale_color_manual(values = colRainbow, name = "", labels = c("Alps to Jura", "Alps to Black Forest", "Alps to Vosges-Palatinate", "Jura to Alps",
                                                     "Jura to Black Forest", "Jura to Vosges-Palatinate", "Black Forest to Alps",
                                                     "Black Forest to Jura", "Black Forest to Vosges-Palatinate", "Vosges-Palatinate to Alps",
                                                     "Vosges-Palatinate to Black Forest", "Vosges-Palatinate to Jura")) +
  #guides(color = guide_legend(ncol = 4)) + 
  #scale_fill_manual(values = colRainbow) +
  annotate("rect", xmin = -Inf, xmax = 3, ymin = -Inf, ymax = Inf, alpha = .7)# +
  #theme(legend.position = "bottom")


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
terrOccMapRas <- raster("appendix_lynxIBM/module/inputs/habMap.tif")
# Rescale the value to obtain a mean over all simulations
terrOccMapRas[] <- of(world = terrOccMap, agents = NetLogoR::patches(terrOccMap)) / nSim
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


