library(NetLogoR)
library(terra)
library(dplyr)
library(sf)

#######################
## MODEL CALIBRATION ##
#######################

pathCalFolder <- "C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/calibration/calibration_phase1"
folders <- list.files(pathCalFolder)
lastYear <- 50

habMapSpaDES <- rast("C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/inputs/habMap.tif")

calibrIBM <- data.frame(cal = 1:length(folders),
                        ## Reference values
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

for(folder in 1:length(folders)){
  listSim <- list.files(paste0(pathCalFolder, "/", folders[folder]))
  nSim <- length(listSim)
  
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
    load(paste0(pathCalFolder, "/", folders[folder], "/", listSim[file]))
    
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
  
  # Summary per calibration
  # Mortality Alps
  calibrIBM[folder, "mean_rAllRes_Alps"] <- mean(rAllRes_Alps, na.rm = TRUE)
  calibrIBM[folder, "CI_inf_rAllRes_Alps"] <- t.test(rAllRes_Alps, na.rm = TRUE)$conf.int[1]
  calibrIBM[folder, "CI_sup_rAllRes_Alps"] <- t.test(rAllRes_Alps, na.rm = TRUE)$conf.int[2]
  calibrIBM[folder, "mean_rAllDisp_Alps"] <- mean(rAllDisp_Alps, na.rm = TRUE)
  calibrIBM[folder, "CI_inf_rAllDisp_Alps"] <- t.test(rAllDisp_Alps, na.rm = TRUE)$conf.int[1]
  calibrIBM[folder, "CI_sup_rAllDisp_Alps"] <- t.test(rAllDisp_Alps, na.rm = TRUE)$conf.int[2]
  calibrIBM[folder, "mean_rCollRes_Alps"] <- mean(rCollRes_Alps, na.rm = TRUE)
  calibrIBM[folder, "CI_inf_rCollRes_Alps"] <- t.test(rCollRes_Alps, na.rm = TRUE)$conf.int[1]
  calibrIBM[folder, "CI_sup_rCollRes_Alps"] <- t.test(rCollRes_Alps, na.rm = TRUE)$conf.int[2]
  calibrIBM[folder, "mean_rCollDisp_Alps"] <- mean(rCollDisp_Alps, na.rm = TRUE)
  calibrIBM[folder, "CI_inf_rCollDisp_Alps"] <- t.test(rCollDisp_Alps, na.rm = TRUE)$conf.int[1]
  calibrIBM[folder, "CI_sup_rCollDisp_Alps"] <- t.test(rCollDisp_Alps, na.rm = TRUE)$conf.int[2]
  calibrIBM[folder, "mean_nCollAlps"] <- mean(nCollAlps, na.rm = TRUE)
  calibrIBM[folder, "CI_inf_nCollAlps"] <- t.test(nCollAlps, na.rm = TRUE)$conf.int[1]
  calibrIBM[folder, "CI_sup_nCollAlps"] <- t.test(nCollAlps, na.rm = TRUE)$conf.int[2]
  # Mortality Jura
  calibrIBM[folder, "mean_rAllRes_Jura"] <- mean(rAllRes_Jura, na.rm = TRUE)
  calibrIBM[folder, "CI_inf_rAllRes_Jura"] <- t.test(rAllRes_Jura, na.rm = TRUE)$conf.int[1]
  calibrIBM[folder, "CI_sup_rAllRes_Jura"] <- t.test(rAllRes_Jura, na.rm = TRUE)$conf.int[2]
  calibrIBM[folder, "mean_rAllDisp_Jura"] <- mean(rAllDisp_Jura, na.rm = TRUE)
  calibrIBM[folder, "CI_inf_rAllDisp_Jura"] <- t.test(rAllDisp_Jura, na.rm = TRUE)$conf.int[1]
  calibrIBM[folder, "CI_sup_rAllDisp_Jura"] <- t.test(rAllDisp_Jura, na.rm = TRUE)$conf.int[2]
  calibrIBM[folder, "mean_rCollRes_Jura"] <- mean(rCollRes_Jura, na.rm = TRUE)
  calibrIBM[folder, "CI_inf_rCollRes_Jura"] <- t.test(rCollRes_Jura, na.rm = TRUE)$conf.int[1]
  calibrIBM[folder, "CI_sup_rCollRes_Jura"] <- t.test(rCollRes_Jura, na.rm = TRUE)$conf.int[2]
  calibrIBM[folder, "mean_rCollDisp_Jura"] <- mean(rCollDisp_Jura, na.rm = TRUE)
  calibrIBM[folder, "CI_inf_rCollDisp_Jura"] <- t.test(rCollDisp_Jura, na.rm = TRUE)$conf.int[1]
  calibrIBM[folder, "CI_sup_rCollDisp_Jura"] <- t.test(rCollDisp_Jura, na.rm = TRUE)$conf.int[2]
  calibrIBM[folder, "mean_nCollJura"] <- mean(nCollJura, na.rm = TRUE)
  calibrIBM[folder, "CI_inf_nCollJura"] <- t.test(nCollJura, na.rm = TRUE)$conf.int[1]
  calibrIBM[folder, "CI_sup_nCollJura"] <- t.test(nCollJura, na.rm = TRUE)$conf.int[2]
  # Reproduction
  calibrIBM[folder, "mean_rRepro"] <- mean(pRepro, na.rm = TRUE)
  calibrIBM[folder, "CI_inf_rRepro"] <- t.test(pRepro, na.rm = TRUE)$conf.int[1]
  calibrIBM[folder, "CI_sup_rRepro"] <- t.test(pRepro, na.rm = TRUE)$conf.int[2]
  # Dispersal distance
  calibrIBM[folder, "mean_distDispAlps"] <- mean(distDispAlps, na.rm = TRUE) / 1000
  calibrIBM[folder, "CI_inf_distDispAlps"] <- t.test(distDispAlps, na.rm = TRUE)$conf.int[1] / 1000
  calibrIBM[folder, "CI_sup_distDispAlps"] <- t.test(distDispAlps, na.rm = TRUE)$conf.int[2]/ 1000
  calibrIBM[folder, "mean_distDispJura"] <- mean(distDispJura, na.rm = TRUE) / 1000
  calibrIBM[folder, "CI_inf_distDispJura"] <- t.test(distDispJura, na.rm = TRUE)$conf.int[1] / 1000
  calibrIBM[folder, "CI_sup_distDispJura"] <- t.test(distDispJura, na.rm = TRUE)$conf.int[2] / 1000

  print(folder)
  save(calibrIBM, file = "C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/calibration/calibrIBM_phase1.RData")
}


# Select the simulations that match the patterns
# The mean needs to be inside the CI for the mortality patterns
matchingCal <- calibrIBM[calibrIBM$mean_rAllRes_Alps <= calibrIBM$CI_sup_rAllRes_Alps_ref & calibrIBM$mean_rAllRes_Alps >= calibrIBM$CI_inf_rAllRes_Alps_ref &
                           calibrIBM$mean_rAllDisp_Alps <= calibrIBM$CI_sup_rAllDisp_Alps_ref & calibrIBM$mean_rAllDisp_Alps >= calibrIBM$CI_inf_rAllDisp_Alps_ref &
                           calibrIBM$mean_rCollRes_Alps <= calibrIBM$CI_sup_rCollRes_Alps_ref & calibrIBM$mean_rCollRes_Alps >= calibrIBM$CI_inf_rCollRes_Alps_ref &
                           calibrIBM$mean_rCollDisp_Alps <= calibrIBM$CI_sup_rCollDisp_Alps_ref & calibrIBM$mean_rCollDisp_Alps >= calibrIBM$CI_inf_rCollDisp_Alps_ref &
                           calibrIBM$mean_nCollAlps <= calibrIBM$CI_sup_nCollAlps_ref & calibrIBM$mean_nCollAlps >= calibrIBM$CI_inf_nCollAlps_ref &
                           
                           calibrIBM$mean_rAllRes_Jura <= calibrIBM$CI_sup_rAllRes_Jura_ref & calibrIBM$mean_rAllRes_Jura >= calibrIBM$CI_inf_rAllRes_Jura_ref &
                           calibrIBM$mean_rAllDisp_Jura <= calibrIBM$CI_sup_rAllDisp_Jura_ref & calibrIBM$mean_rAllDisp_Jura >= calibrIBM$CI_inf_rAllDisp_Jura_ref &
                           calibrIBM$mean_rCollRes_Jura <= calibrIBM$CI_sup_rCollRes_Jura_ref & calibrIBM$mean_rCollRes_Jura >= calibrIBM$CI_inf_rCollRes_Jura_ref &
                           calibrIBM$mean_rCollDisp_Jura <= calibrIBM$CI_sup_rCollDisp_Jura_ref & calibrIBM$mean_rCollDisp_Jura >= calibrIBM$CI_inf_rCollDisp_Jura_ref &
                           calibrIBM$mean_nCollJura <= calibrIBM$CI_sup_nCollJura_ref & calibrIBM$mean_nCollJura >= calibrIBM$CI_inf_nCollJura_ref #&
                           
                           #calibrIBM$mean_distDispAlps <= calibrIBM$CI_sup_distDispAlps_ref & calibrIBM$mean_distDispAlps >= calibrIBM$CI_inf_distDispAlps_ref &
                           #calibrIBM$mean_distDispJura <= calibrIBM$CI_sup_distDispJura_ref & calibrIBM$mean_distDispJura >= calibrIBM$CI_inf_distDispJura_ref 
                           ,]
nrow(matchingCal)

# Second, among the one matching the patterns, select the one where the means are the closest
# Change qTresh from 1 to 0 until selecting the best xxx simulations
qTresh <- 0.74
selectedCal <- matchingCal[abs(matchingCal$mean_rAllRes_Alps_ref - matchingCal$mean_rAllRes_Alps) <= quantile(abs(matchingCal$mean_rAllRes_Alps_ref - matchingCal$mean_rAllRes_Alps), qTresh) &
                             abs(matchingCal$mean_rAllDisp_Alps_ref - matchingCal$mean_rAllDisp_Alps) <= quantile(abs(matchingCal$mean_rAllDisp_Alps_ref - matchingCal$mean_rAllDisp_Alps), qTresh) &
                             abs(matchingCal$mean_rCollRes_Alps_ref - matchingCal$mean_rCollRes_Alps) <= quantile(abs(matchingCal$mean_rCollRes_Alps_ref - matchingCal$mean_rCollRes_Alps), qTresh) &
                             abs(matchingCal$mean_rCollDisp_Alps_ref - matchingCal$mean_rCollDisp_Alps) <= quantile(abs(matchingCal$mean_rCollDisp_Alps_ref - matchingCal$mean_rCollDisp_Alps), qTresh) &
                             abs(matchingCal$mean_nCollAlps_ref - matchingCal$mean_nCollAlps) <= quantile(abs(matchingCal$mean_nCollAlps_ref - matchingCal$mean_nCollAlps), qTresh) &
                             
                             abs(matchingCal$mean_rAllRes_Jura_ref - matchingCal$mean_rAllRes_Jura) <= quantile(abs(matchingCal$mean_rAllRes_Jura_ref - matchingCal$mean_rAllRes_Jura), qTresh) &
                             abs(matchingCal$mean_rAllDisp_Jura_ref - matchingCal$mean_rAllDisp_Jura) <= quantile(abs(matchingCal$mean_rAllDisp_Jura_ref - matchingCal$mean_rAllDisp_Jura), qTresh) &
                             abs(matchingCal$mean_rCollRes_Jura_ref - matchingCal$mean_rCollRes_Jura) <= quantile(abs(matchingCal$mean_rCollRes_Jura_ref - matchingCal$mean_rCollRes_Jura), qTresh) &
                             abs(matchingCal$mean_rCollDisp_Jura_ref - matchingCal$mean_rCollDisp_Jura) <= quantile(abs(matchingCal$mean_rCollDisp_Jura_ref - matchingCal$mean_rCollDisp_Jura), qTresh) &
                             abs(matchingCal$mean_nCollJura_ref - matchingCal$mean_nCollJura) <= quantile(abs(matchingCal$mean_nCollJura_ref - matchingCal$mean_nCollJura), qTresh) &
                             
                             abs(matchingCal$rRepro_ref - matchingCal$rRepro) <= quantile(abs(matchingCal$rRepro_ref - matchingCal$rRepro), qTresh) &
                             abs(matchingCal$mean_distDispAlps_ref - matchingCal$mean_distDispAlps) <= quantile(abs(matchingCal$mean_distDispAlps_ref - matchingCal$mean_distDispAlps), qTresh) &
                             abs(matchingCal$mean_distDispJura_ref - matchingCal$mean_distDispJura) <= quantile(abs(matchingCal$mean_distDispJura_ref - matchingCal$mean_distDispJura), qTresh),]
nrow(selectedCal)
