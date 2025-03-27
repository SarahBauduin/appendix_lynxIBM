# Explore the sensitivity of the parameters
# Using the simulations from the calibration phase 2 

library(NetLogoR)

load("C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/calibration/calibrIBM_phase2.RData")

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


# Read all simulated files
pathCalFolder <- "C:/Users/sarah.bauduin/Documents/GitHub/appendix_lynxIBM/module/calibration/calibration_phase2"
folders <- list.files(pathCalFolder)
# Select the simulation which produced patterns that were within the ranges of the pattern reference values
selected_folder <- folders[as.numeric(row.names(matchingCal))]
# Or select all calibrations from phase 2
selected_folder <- folders

lastYear <- 50
nSim <- 15

# Prepare the table to store the number of individuals per population
popAlps <- cbind.data.frame(repFolder = rep(selected_folder, each = (lastYear+1) * nSim), 
                 repSim = as.numeric(rep(rep(1:nSim, each = (lastYear+1)), nrow(matchingCal))), 
                 year = as.numeric(rep(1:(lastYear+1), nSim * nrow(matchingCal))), 
                 nInd = as.numeric(rep(0, (lastYear+1) * nSim * nrow(matchingCal))))
popJura <- popAlps
popVP <- popAlps
popBF <- popAlps

for(folder in 1:length(selected_folder)){
  listSim <- list.files(paste0(pathCalFolder, "/", selected_folder[folder]))

  for(i in 1:length(listSim)){
    load(paste0(pathCalFolder, "/", selected_folder[folder], "/", listSim[i]))
    
    for(y in 1:(lastYear+1)){
      
      indAlps <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$outputLynx[[y]],
                           agents = NLwith(agents = NetLogoR::patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 1))
      popAlps[popAlps[, "repFolder"] == selected_folder[folder] & popAlps[, "year"] == y & popAlps[, "repSim"] == i, "nInd"] <- NLcount(indAlps) 
      
      indJura <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$outputLynx[[y]],
                           agents = NLwith(agents = NetLogoR::patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 2))
      popJura[popJura[, "repFolder"] == selected_folder[folder] & popJura[, "year"] == y & popJura[, "repSim"] == i, "nInd"] <- NLcount(indJura) 
      
      indVosPal <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$outputLynx[[y]],
                             agents = NLwith(agents = NetLogoR::patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 3))
      popVP[popVP[, "repFolder"] == selected_folder[folder] & popVP[, "year"] == y & popVP[, "repSim"] == i, "nInd"] <- NLcount(indVosPal) 
      
      indBF <- turtlesOn(world = lynxIBMrun$popDist, turtles = lynxIBMrun$outputLynx[[y]],
                         agents = NLwith(agents = NetLogoR::patches(lynxIBMrun$popDist), world = lynxIBMrun$popDist, val = 4))
      popBF[popBF[, "repFolder"] == selected_folder[folder] & popBF[, "year"] == y & popBF[, "repSim"] == i, "nInd"] <- NLcount(indBF)
    }
    print(i)
    
  }
  print(folder)
  
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

##################################################
# Growth rates for all calibrations mixed together
# Calculate the mean and 95% confidence intervals of the growth rates per year and per population
library(Rmisc)
allPopSum <- summarySE(allPop, measurevar = "r", groupvars = c("year", "pop"), na.rm = TRUE)
colnames(allPopSum)[2] <- "Populations"
library(ggplot2)
ggplot(allPopSum, aes(x = year, y = r, colour = Populations)) + 
  geom_ribbon(aes(ymin = r - ci, ymax = r + ci, x = year, 
                  fill = Populations), alpha = 0.3) +
  geom_line() +
  geom_point() +
  coord_cartesian(ylim = c(0.9, 1.5)) +
  annotate("rect", xmin = -Inf, xmax = 3, ymin = -Inf, ymax = Inf, alpha = .7) +
  labs(x ="Years simulated", y = "Population growth rates")

# Population sizes for all calibrations mixed together
# Calculate the mean and 95% confidence intervals of the population sizes per year and per population
allPopSum <- summarySE(allPop, measurevar = "nInd", groupvars = c("year", "pop"), na.rm = TRUE)
colnames(allPopSum)[2] <- "Populations"
ggplot(allPopSum, aes(x = year, y = nInd, color = Populations)) + 
  geom_ribbon(aes(ymin = nInd - ci, ymax= nInd + ci, x= year, 
                  fill = Populations), alpha = 0.3) +
  geom_line() +
  geom_point() +
  annotate("rect", xmin = -Inf, xmax = 3, ymin = -Inf, ymax = Inf, alpha = .7) +
  labs(x ="Years simulated", y = "Population growth rates")

######################################

######################################
# Growth rate per calibration scenario
allPopSum <- summarySE(allPop, measurevar = "r", groupvars = c("year", "pop", "repFolder"), na.rm = TRUE)
colnames(allPopSum)[2] <- "Populations"
# Plot all populations together
ggplot(allPopSum, aes(x = year, y = r, group = interaction(repFolder, Populations))) + 
  geom_ribbon(aes(ymin = r - ci, ymax = r + ci, x = year, 
                  fill = Populations), alpha = 0.15) +
  geom_line(aes(color = Populations)) +
  geom_point(aes(color = Populations)) +
  #coord_cartesian(ylim = c(0.9, 1.5)) +
  annotate("rect", xmin = -Inf, xmax = 3, ymin = -Inf, ymax = Inf, alpha = .7) +
  labs(x ="Years simulated", y = "Population growth rates")

# Plot Alps population
ggplot(allPopSum[allPopSum$Populations == "Alps",], aes(x = year, y = r, group = repFolder)) + 
  geom_ribbon(aes(ymin = r - ci, ymax = r + ci, x = year),
              alpha = 0.15, fill = "#F8766D") +
  geom_line(color = "#F8766D") +
  geom_point(color = "#F8766D") +
  # Selected value
  geom_ribbon(data = allPopSum[allPopSum$Populations == "Alps" & allPopSum$repFolder == "cal15",], 
              mapping = aes(ymin = r - ci, ymax = r + ci, x = year), 
              alpha = 0.2, fill = "black") +
  geom_line(data = allPopSum[allPopSum$Populations == "Alps" & allPopSum$repFolder == "cal15",], 
            color = "black") +
  geom_point(data = allPopSum[allPopSum$Populations == "Alps" & allPopSum$repFolder == "cal15",], 
             color = "black") +
  # Growth rate = 1
  geom_line(data = cbind.data.frame(year = 1:51, GR = 1, repFolder = 0), 
            mapping = aes(x = year, y = GR), color = "grey10", linetype = "dashed") +
  coord_cartesian(ylim = c(0.9, 1.2)) +
  annotate("rect", xmin = -Inf, xmax = 3, ymin = -Inf, ymax = Inf, alpha = .7) +
  labs(x ="Years simulated", y = "Population growth rates")
# Calculate the mean % difference with the selected calibration
maxR <- allPopSum[allPopSum$Populations == "Alps",] %>% 
  group_by(year) %>%
  summarise(maxR = max(r)) %>% 
  select(maxR) %>% 
  as.vector(.) 
mean(abs(((maxR$maxR - allPopSum[allPopSum$Populations == "Alps" & allPopSum$repFolder == "cal15", "r"]) /
            allPopSum[allPopSum$Populations == "Alps" & allPopSum$repFolder == "cal15", "r"]) * 100), na.rm = TRUE)
minR <- allPopSum[allPopSum$Populations == "Alps",] %>% 
  group_by(year) %>%
  summarise(minR = min(r)) %>% 
  select(minR) %>% 
  as.vector(.)
mean(abs(((minR$minR - allPopSum[allPopSum$Populations == "Alps" & allPopSum$repFolder == "cal15", "r"]) /
            allPopSum[allPopSum$Populations == "Alps" & allPopSum$repFolder == "cal15", "r"]) * 100), na.rm = TRUE)

# Plot Jura population
ggplot(allPopSum[allPopSum$Populations == "Jura",], aes(x = year, y = r, group = repFolder)) + 
  geom_ribbon(aes(ymin = r - ci, ymax = r + ci, x = year), 
              alpha = 0.15, fill = "#7CAE00") +
  geom_line(color = "#7CAE00") +
  geom_point(color = "#7CAE00") +
  # Selected value
  geom_ribbon(data = allPopSum[allPopSum$Populations == "Jura" & allPopSum$repFolder == "cal15",], 
              mapping = aes(ymin = r - ci, ymax = r + ci, x = year), 
              alpha = 0.2, fill = "black") +
  geom_line(data = allPopSum[allPopSum$Populations == "Jura" & allPopSum$repFolder == "cal15",], 
            color = "black") +
  geom_point(data = allPopSum[allPopSum$Populations == "Jura" & allPopSum$repFolder == "cal15",], 
             color = "black") +
  # Growth rate = 1
  geom_line(data = cbind.data.frame(year = 1:51, GR = 1, repFolder = 0), 
            mapping = aes(x = year, y = GR), 
            color = "grey10", linetype = "dashed") +
  coord_cartesian(ylim = c(0.9, 1.2)) +
  annotate("rect", xmin = -Inf, xmax = 3, ymin = -Inf, ymax = Inf, alpha = .7) +
  labs(x ="Years simulated", y = "Population growth rates")
# Calculate the mean % difference with the selected calibration
maxR <- allPopSum[allPopSum$Populations == "Jura",] %>% 
  group_by(year) %>%
  summarise(maxR = max(r)) %>% 
  select(maxR) %>% 
  as.vector(.) 
mean(abs(((maxR$maxR - allPopSum[allPopSum$Populations == "Jura" & allPopSum$repFolder == "cal15", "r"]) /
            allPopSum[allPopSum$Populations == "Jura" & allPopSum$repFolder == "cal15", "r"]) * 100), na.rm = TRUE)
minR <- allPopSum[allPopSum$Populations == "Jura",] %>% 
  group_by(year) %>%
  summarise(minR = min(r)) %>% 
  select(minR) %>% 
  as.vector(.)
mean(abs(((minR$minR - allPopSum[allPopSum$Populations == "Jura" & allPopSum$repFolder == "cal15", "r"]) /
            allPopSum[allPopSum$Populations == "Jura" & allPopSum$repFolder == "cal15", "r"]) * 100), na.rm = TRUE)

# Plot Vosges-Palatinate population
ggplot(allPopSum[allPopSum$Populations == "Vosges-Palatinate",], aes(x = year, y = r, group = repFolder)) + 
  geom_ribbon(aes(ymin = r - ci, ymax = r + ci, x = year),
              alpha = 0.15, fill = "#00BFC4") +
  geom_line(color = "#00BFC4") +
  geom_point(color = "#00BFC4") +
  # Selected value
  geom_ribbon(data = allPopSum[allPopSum$Populations == "Vosges-Palatinate" & allPopSum$repFolder == "cal15",], 
              mapping = aes(ymin = r - ci, ymax = r + ci, x = year),
              alpha = 0.2, fill = "black") +
  geom_line(data = allPopSum[allPopSum$Populations == "Vosges-Palatinate" & allPopSum$repFolder == "cal15",], 
            color = "black") +
  geom_point(data = allPopSum[allPopSum$Populations == "Vosges-Palatinate" & allPopSum$repFolder == "cal15",], 
             color = "black") +
  # Growth rate = 1
  geom_line(data = cbind.data.frame(year = 1:51, GR = 1, repFolder = 0), 
            mapping = aes(x = year, y = GR), 
            color = "grey10", linetype = "dashed") +
  #coord_cartesian(ylim = c(0.9, 1.2)) +
  annotate("rect", xmin = -Inf, xmax = 3, ymin = -Inf, ymax = Inf, alpha = .7) +
  labs(x ="Years simulated", y = "Population growth rates")
# Calculate the mean % difference with the selected calibration
maxR <- allPopSum[allPopSum$Populations == "Vosges-Palatinate",] %>% 
  group_by(year) %>%
  summarise(maxR = max(r)) %>% 
  select(maxR) %>% 
  as.vector(.) 
mean(abs(((maxR$maxR - allPopSum[allPopSum$Populations == "Vosges-Palatinate" & allPopSum$repFolder == "cal15", "r"]) /
            allPopSum[allPopSum$Populations == "Vosges-Palatinate" & allPopSum$repFolder == "cal15", "r"]) * 100), na.rm = TRUE)
minR <- allPopSum[allPopSum$Populations == "Vosges-Palatinate",] %>% 
  group_by(year) %>%
  summarise(minR = min(r)) %>% 
  select(minR) %>% 
  as.vector(.)
mean(abs(((minR$minR - allPopSum[allPopSum$Populations == "Vosges-Palatinate" & allPopSum$repFolder == "cal15", "r"]) /
            allPopSum[allPopSum$Populations == "Vosges-Palatinate" & allPopSum$repFolder == "cal15", "r"]) * 100), na.rm = TRUE)

# Plot Black Forest population
ggplot(allPopSum[allPopSum$Populations == "Black Forest",], aes(x = year, y = r, group = repFolder)) + 
  geom_ribbon(aes(ymin = r - ci, ymax = r + ci, x = year),
              alpha = 0.15, fill = "#C77CFF") +
  geom_line(color = "#C77CFF") +
  geom_point(color = "#C77CFF") +
  # Selected value
  geom_ribbon(data = allPopSum[allPopSum$Populations == "Black Forest" & allPopSum$repFolder == "cal15",], 
              mapping = aes(ymin = r - ci, ymax = r + ci, x = year), 
              alpha = 0.2, fill = "black") +
  geom_line(data = allPopSum[allPopSum$Populations == "Black Forest" & allPopSum$repFolder == "cal15",],
            color = "black") +
  geom_point(data = allPopSum[allPopSum$Populations == "Black Forest" & allPopSum$repFolder == "cal15",],
             color = "black") +
  # Growth rate = 1
  geom_line(data = cbind.data.frame(year = 1:51, GR = 1, repFolder = 0), 
            mapping = aes(x = year, y = GR), 
            color = "grey10", linetype = "dashed") +
  #coord_cartesian(ylim = c(0.9, 1.2)) +
  annotate("rect", xmin = -Inf, xmax = 3, ymin = -Inf, ymax = Inf, alpha = .7) +
  labs(x ="Years simulated", y = "Population growth rates")
# Calculate the mean % difference with the selected calibration
maxR <- allPopSum[allPopSum$Populations == "Black Forest",] %>% 
  group_by(year) %>%
  summarise(maxR = max(r)) %>% 
  select(maxR) %>% 
  as.vector(.) 
mean(abs(((maxR$maxR - allPopSum[allPopSum$Populations == "Black Forest" & allPopSum$repFolder == "cal15", "r"]) /
            allPopSum[allPopSum$Populations == "Black Forest" & allPopSum$repFolder == "cal15", "r"]) * 100), na.rm = TRUE)
minR <- allPopSum[allPopSum$Populations == "Black Forest",] %>% 
  group_by(year) %>%
  summarise(minR = min(r)) %>% 
  select(minR) %>% 
  as.vector(.)
mean(abs(((minR$minR - allPopSum[allPopSum$Populations == "Black Forest" & allPopSum$repFolder == "cal15", "r"]) /
            allPopSum[allPopSum$Populations == "Black Forest" & allPopSum$repFolder == "cal15", "r"]) * 100), na.rm = TRUE)

##########################################

##########################################
# Population size per calibration scenario
allPopSum <- summarySE(allPop, measurevar = "nInd", groupvars = c("year", "pop", "repFolder"), na.rm = TRUE)
colnames(allPopSum)[2] <- "Populations"
ggplot(allPopSum, aes(x = year, y = nInd, group = interaction(repFolder, Populations))) + 
  geom_ribbon(aes(ymin = nInd - ci, ymax = nInd + ci, x = year, fill = Populations), alpha = 0.15) +
  geom_line(aes(color = Populations)) +
  geom_point(aes(color = Populations)) +
  geom_ribbon(data = allPopSum[allPopSum$repFolder == "cal15",], 
              mapping = aes(ymin = nInd - ci, ymax = nInd + ci, x = year, group = Populations), 
              alpha = 0.3, fill = "black") +
  geom_line(data = allPopSum[allPopSum$repFolder == "cal15",], 
            color = "black") +
  geom_point(data = allPopSum[allPopSum$repFolder == "cal15",], 
             color = "black", alpha = 0.5) +
  annotate("rect", xmin = -Inf, xmax = 3, ymin = -Inf, ymax = Inf, alpha = .7) +
  labs(x ="Years simulated", y = "Population growth rates")

##########################################