## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "lynxIBM",
  description = "Modeling of the lynx dispersal and population dynamics while including habitat
  preference and road mortality",
  keywords = c("lynx", "movement", "habitat", "collisions"),
  authors = person("Sarah", "Bauduin", email = "sarah.bauduin@ofb.gouv.fr", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.7", lynxIBM = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "lynxIBM.Rmd")),
  reqdPkgs = list("NetLogoR", "testthat", "SpaDES", "raster", "randomcoloR", "data.table", "dplyr", "doBy"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter("pRepro", "numeric", 0.95, NA, NA, "Probability of reproduction for concerned females - Parameter calibrated with simulations"),
    defineParameter("nKittyYoungF", "numeric", c(1, 2), NA, NA, "Number of kittens young females can have if they do reproduce"),
    defineParameter("pKittyYoungF", "numeric", c(0.5, 0.5), NA, NA, "Probabilities for young females to have nKittyYoungF"),
    defineParameter("maxAgeYoungF", "numeric", 11, NA, NA, "Age maximum to be considered 'young female' for reproduction"),
    defineParameter("nKittyOldF", "numeric", c(0, 1), NA, NA, "Number of kittens old females can have if they do reproduce"),
    defineParameter("pKittyOldF", "numeric", c(0.5, 0.5), NA, NA, "Probabilities for old females to have nKittyOldF"),
    defineParameter("minAgeReproF", "numeric", 2, NA, NA, "Age minimum for females to reproduce"),
    defineParameter("minAgeReproM", "numeric", 3, NA, NA, "Age minimum for males to reproduce"),
    defineParameter("pMortResAlps", "numeric", 0.17, NA, NA, "Fixed annual probability of mortality for residents in the Alps - Parameter calibrated with simulations"),
    defineParameter("pMortResJura", "numeric", 0.1, NA, NA, "Fixed annual probability of mortality for residents in the Jura - Parameter calibrated with simulations"),
    defineParameter("pMortResVosgesPalatinate", "numeric", 0.1, NA, NA, "Fixed annual probability of mortality for residents in the Vosges-Palatinate - Parameter calibrated with simulations"),
    defineParameter("pMortResBlackForest", "numeric", 0.1, NA, NA, "Fixed annual probability of mortality for residents in the Black Forest - Parameter calibrated with simulations"),
    defineParameter("ageMax", "numeric", 20, NA, NA, "Age maximum individuals can be"),
    defineParameter("xPs", "numeric", 11, NA, NA, "Exponent of power function to define the daily step distribution"),
    defineParameter("sMaxPs", "numeric", 45, NA, NA, "Maximum number of intraday movement steps"),
    defineParameter("pMat", "numeric", 0.03, NA, NA, "Probability of stepping into matrix cells"),
    defineParameter("pCorr", "numeric", 0.5, NA, NA, "Movement correlation probability"),
    defineParameter("pMortDispAlps", "numeric", 0.0007, NA, NA, "Fixed daily probability of mortality for dispersers in the Alps - Parameter calibrated with simulations"),
    defineParameter("pMortDispJura", "numeric", 0.0012, NA, NA, "Fixed daily probability of mortality for dispersers in the Jura - Parameter calibrated with simulations"),
    defineParameter("pMortDispVosgesPalatinate", "numeric", 0.0012, NA, NA, "Fixed daily probability of mortality for dispersers in the Vosges-Palatinate - Parameter calibrated with simulations"),
    defineParameter("pMortDispBlackForest", "numeric", 0.0012, NA, NA, "Fixed daily probability of mortality for dispersers in the Black Forest - Parameter calibrated with simulations"),
    defineParameter("corrFactorRes", "numeric", 2.5, NA, NA, "Correction factor for road mortality risk for residents - Parameter calibrated with simulations"),
    defineParameter("corrFactorDisp", "numeric", 125, NA, NA, "Correction factor for road mortality risk for dispersers - Parameter calibrated with simulations"),
    defineParameter("nMatMax", "numeric", 10, NA, NA, "Maximum number of consecutive steps within which the individual needs to find dispsersal habitat"),
    defineParameter("coreTerrSizeFAlps", "numeric", 97, NA, NA, "Core size for a female territory (km2) in the Alps"),
    defineParameter("coreTerrSizeFJura", "numeric", 126, NA, NA, "Core size for a female territory (km2) in the Jura"),
    defineParameter("coreTerrSizeFVosgesPalatinate", "numeric", 126, NA, NA, "Core size for a female territory (km2) in the Vosges-Palatinate"),
    defineParameter("coreTerrSizeFBlackForest", "numeric", 126, NA, NA, "Core size for a female territory (km2) in the Black Forest"),
    defineParameter("terrSizeMAlps", "numeric", 159, NA, NA, "Territory size for a male (km2) in the Alps"),
    defineParameter("terrSizeMJura", "numeric", 270, NA, NA, "Territory size for a male (km2) in the Jura"),
    defineParameter("terrSizeMVosgesPalatinate", "numeric", 270, NA, NA, "Territory size for a male (km2) in the Vosges-Palatinate"),
    defineParameter("terrSizeMBlackForest", "numeric", 270, NA, NA, "Territory size for a male (km2) in the Black Forest"),
    defineParameter("testON", "logical", TRUE, NA, NA, "Run the tests")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

##############
## Event types
##
doEvent.lynxIBM = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      
      sim <- initSim(sim)
      
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "lynxIBM", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "lynxIBM", "save")
      sim <- scheduleEvent(sim, start(sim, "year") + 0.00001, "lynxIBM", "daily")
      sim <- scheduleEvent(sim, time(sim, "year") + 0.99999, "lynxIBM", "yearEnd")
      sim$day <- 1
    },
    plot = {
      
      if(NLcount(sim$lynx) != 0){
        sim <- plotSim(sim)
      }
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "lynxIBM", "plot")
    },
    save = {
      
    },

    yearEnd = {
      
      #####
      if(NLcount(sim$lynx) != 0){
        sim <- demography(sim)
      }
      if(NLcount(sim$lynx) != 0){
        sim <- reproduction(sim)
      }
      if(NLcount(sim$lynx) != 0){
        sim <- mortality(sim) # annual mortality
      }        

      #####
      
      sim <- saveSimYearly(sim)
      sim <- scheduleEvent(sim, time(sim, "year") + 1, "lynxIBM", "yearEnd")
      sim$day <- 1
    },
    daily = {
      
      if(NLcount(sim$lynx) != 0){
        if(NLcount(NLwith(agents = sim$lynx, var = "status", val = "disp")) != 0) {
          sim$tempSaveLynx <- NLwith(agents = sim$lynx, var = "status", val = "disp") # for the daily save
          sim <- dispersal(sim)
          sim <- saveSimDaily(sim)
        }
      }
      sim <- scheduleEvent(sim, time(sim, "year") + 1/365, "lynxIBM", "daily")
      sim$day <- sim$day + 1
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}


##################
## Event functions

### Init
initSim <- function(sim) {
  
  # Habitat map as a worldMatrix
  # Habitat categories
  # Barrier = 0
  # Matrice = 2
  # Dispersing = 3
  # Breeding = 4
  habitatRaster <- sim$habMapSpaDES # input raster of the different habitat categories for the lynx
  habitatRaster[is.na(habitatRaster)] <- 0 # barrier on the edge of the world that is not defined
  sim$habitatMap <- createWorld(minPxcor = 1,
                                maxPxcor = NCOL(habitatRaster),
                                minPycor = 1,
                                maxPycor = NROW(habitatRaster),
                                data = values(habitatRaster))

  # Road mortality map as a worldMatrix
  roadMortRaster <- sim$collProbSpaDES # input raster of the collision probabilities
  sim$roadMortMap <- createWorld(minPxcor = 1,
                                 maxPxcor = NCOL(roadMortRaster),
                                 minPycor = 1,
                                 maxPycor = NROW(roadMortRaster),
                                 data = values(roadMortRaster))
  
  # Distribution of the population as a worldMatrix
  # Alps = 1
  # Jura = 2
  # Vosges-Palatinate = 3
  # Black Forest = 4
  populationDist <- sim$fourPopSpaDES # input raster of the population distribution on the landscape
  sim$popDist <- createWorld(minPxcor = 1,
                             maxPxcor = NCOL(populationDist),
                             minPycor = 1,
                             maxPycor = NROW(populationDist),
                             data = values(populationDist))
  
  # Individuals going out of the landscape die
  # The mortality probability on landscape borders needs to be = 1
  allPatches <- patches(sim$habitatMap)
  borders <- allPatches[allPatches[, "pxcor"] %in% c(minPxcor(sim$habitatMap),
                                                     maxPxcor(sim$habitatMap)) |
                          allPatches[, "pycor"] %in% c(minPycor(sim$habitatMap),
                                                       maxPycor(sim$habitatMap)), ]
  # The "borders" of the world are patches with data values in them (no collisions probabilities)
  # Need to replace NA (world borders not defined) by 1 and all the neighboring cells of these patches to have a line of cells along the borders with mortality = 1
  # so that individuals won't have NA values in cell type (from the habitat map) when doing neighbors because they would have died before if going too close of the borders
  bordersNA <- NLwith(agents = patches(sim$roadMortMap), world = sim$roadMortMap, val = NA)
  bordersAll <- patchSet(borders, bordersNA) #remove the duplicates
  # Patches neighbors of these patches = 1 row of cell
  bordersPlus <- NetLogoR::neighbors(world = sim$roadMortMap, agents = bordersAll, nNeighbors = 8, torus = FALSE)
  # Need a second row of patches transformed into 1 otherwise on the border, still NA for the landscape
  bordersPlus2rows <- NetLogoR::neighbors(world = sim$roadMortMap, agents = unique(bordersPlus[, c(1, 2)]), nNeighbors = 8, torus = FALSE) 
  sim$roadMortMap <- NLset(world = sim$roadMortMap, agents = unique(bordersPlus2rows[, c(1, 2)]), val = 1)
  
  # Territory map
  sim$terrMap <- createWorld(minPxcor = minPxcor(sim$habitatMap),
                             maxPxcor = maxPxcor(sim$habitatMap),
                             minPycor = minPycor(sim$habitatMap),
                             maxPycor = maxPycor(sim$habitatMap),
                             data = as.numeric(NA))
  
  # Available cells for females to build territories
  availCells <- createWorld(minPxcor = minPxcor(sim$habitatMap),
                            maxPxcor = maxPxcor(sim$habitatMap),
                            minPycor = minPycor(sim$habitatMap),
                            maxPycor = maxPycor(sim$habitatMap),
                            data = 0)
  availCells <- NLset(world = availCells, agents = NLwith(world = sim$habitatMap, agents = allPatches, val = 4), val = 1)
  sim$availCellsRas <- world2raster(availCells)
  
  # Lynx population
  sim$lynx <- spdf2turtles(sim$popInitSpaDES) # input SPDF of individual location, ID, pop, sex and age
  # Renaming the 4 populations
  sim$lynx <- NLset(turtles = sim$lynx,
                    agents = NLwith(agents = sim$lynx, var = "pop", val = c("France_Alps", "Switzerland_Alps")),
                    var = "pop", val = "Alps")
  sim$lynx <- NLset(turtles = sim$lynx,
                    agents = NLwith(agents = sim$lynx, var = "pop", val = c("France_Jura", "Switzerland_Jura")),
                    var = "pop", val = "Jura")
  sim$lynx <- NLset(turtles = sim$lynx,
                    agents = NLwith(agents = sim$lynx, var = "pop", val = c("France_Vosges", "Germany_Palatinat")),
                    var = "pop", val = "Vosges-Palatinate")
  sim$lynx <- NLset(turtles = sim$lynx,
                    agents = NLwith(agents = sim$lynx, var = "pop", val = c("Germany_Bade")),
                    var = "pop", val = "BlackForest")
  sim$lynx@.Data <- subset(sim$lynx@.Data, select = -ID) # remove the "ID" column
  # Coordinates of the lynx regarding the cellnumber of the cells they ae on (their location will be round number = patches coordinates)
  rasterCellNumber <- habitatRaster
  rasterCellNumber[] <- 1:length(rasterCellNumber)
  lynxCellNumber <- raster::extract(rasterCellNumber, sim$popInitSpaDES)
  lynxCoord <- PxcorPycorFromCell(world = sim$habitatMap, cellNum = lynxCellNumber)
  colnames(lynxCoord) <- c("xcor", "ycor")
  sim$lynx <- NLset(turtles = sim$lynx, agents = sim$lynx, var = c("xcor", "ycor"), val = lynxCoord)
  # If some individuals are on 'matrice' (= 2) or 'barrier' (= 0) habitat, they will be relocated on a breeding (= 4) or dispersing (= 3) habitat
  patchLynx <- patchHere(world = sim$habitatMap, turtles = sim$lynx)
  habHereLynx <- of(world = sim$habitatMap, agents = patchLynx) # habitats were the lynx are
  if(length(habHereLynx[habHereLynx %in% c(0, 2)]) != 0){ # if there are some lynx on barrier or matrice habitats
    allGoodHab <- NLwith(world = sim$habitatMap, agents = patches(sim$habitatMap), val = c(3, 4)) # find the dispersing and breeding habitats
    distGoodHab <- NLdist(agents = patchLynx[habHereLynx %in% c(0, 2), , drop = FALSE], agents2 = allGoodHab,
                          world = sim$habitatMap, torus = FALSE, allPairs = TRUE)
    if(class(distGoodHab) == "matrix"){ # more than 1 individual = several patches = matrix
      closestDist <- apply(distGoodHab, 1, FUN = which.min)
      closestPatch <- allGoodHab[closestDist, , drop = FALSE] # find for each individual the closest patch of breeding or dispersing habitat
    } else{ # 1 indivivual = one patch = numerical vector
      closestPatch <- allGoodHab[which.min(distGoodHab), , drop = FALSE]
    }
    patchLynxGood <- patchLynx
    patchLynxGood[habHereLynx %in% c(0, 2), 1] <- closestPatch[, 1] # replace these new locations among all lynx locations
    patchLynxGood[habHereLynx %in% c(0, 2), 2] <- closestPatch[, 2]
    sim$lynx <- moveTo(turtles = sim$lynx, agents = patchLynxGood)
  }
  
  # Test
  if(P(sim)$testON == TRUE) {
    newPatchLynx <- patchHere(world = sim$habitatMap, turtles = sim$lynx)
    newHabHereLynx <- of(world = sim$habitatMap, agents = newPatchLynx)
    expect_equal(length(newHabHereLynx[newHabHereLynx %in% c(0, 2)]), 0)
  }
  
  # Update the lynx object by creating the necessary variables
  sim$lynx <- NLset(turtles = sim$lynx, agents = sim$lynx, var = "heading",
                    val = sample(c(0, 45, 90, 135, 180, 225, 270, 315), size = NLcount(sim$lynx), replace = TRUE))
  # Some individuals do not have information (NA) for sex and age
  sim$lynx <- NLset(turtles = sim$lynx, agents = other(sim$lynx,except=NLwith(agents = sim$lynx,var="sex",val=c("F","M"))), var = "sex",
                    val = sample(c("F", "M"), size = NLcount(other(sim$lynx,except=NLwith(agents = sim$lynx,var="sex",val=c("F","M")))), replace = TRUE))
  sim$lynx <- NLset(turtles = sim$lynx, agents = sim$lynx[is.na(sim$lynx@.Data[, "age"]), ], var = "age",
                    val = sample(2:15, size = NLcount(sim$lynx[is.na(sim$lynx@.Data[, "age"]), ]), replace = TRUE))
  sim$lynx <- turtlesOwn(turtles = sim$lynx, tVar = "status", tVal = "disp")
  sim$lynx <- turtlesOwn(turtles = sim$lynx, tVar = "steps", tVal = 0)
  sim$lynx <- turtlesOwn(turtles = sim$lynx, tVar = "lastDispX", tVal = sim$lynx@.Data[, "xcor"])
  sim$lynx <- turtlesOwn(turtles = sim$lynx, tVar = "lastDispY", tVal = sim$lynx@.Data[, "ycor"])
  sim$lynx <- turtlesOwn(turtles = sim$lynx, tVar = "nMat", tVal = 0)
  sim$lynx <- turtlesOwn(turtles = sim$lynx, tVar = "maleID", tVal = as.numeric(NA))
  sim$lynx <- turtlesOwn(turtles = sim$lynx, tVar = "nFem", tVal = 0)
  sim$lynx <- turtlesOwn(turtles = sim$lynx, tVar = "rdMortTerr", tVal = 0)
  femLynx <- NLwith(agents = sim$lynx, var = "sex", val = "F")

  # Distribution of daily number of steps
  sim$Ps <- (1 / sum(sapply(1:P(sim)$sMaxPs, function(sPs)
    (1 - ((sPs - 1) / (P(sim)$sMaxPs - 1))) ^ P(sim)$xPs))) *
    ((1 - (((1:P(sim)$sMaxPs) - 1) / (P(sim)$sMaxPs - 1))) ^ P(sim)$xPs)
  
  # Keep in memory the state of the population at each time step
  sim$outputLynx <- list()
  sim$outputLynx[[1]] <- sim$lynx
  sim$outputTerrMap <- list()
  sim$outputTerrMap[[1]] <- sim$terrMap
  sim$connectivityMap <- NLset(world = sim$habitatMap, agents = patches(sim$habitatMap), val = 0)
  sim$nColl <- data.frame(ncoll = numeric(), time = numeric())
  sim$bornLynx <- rep(list(sim$lynx[0, ]), times(sim)$end[1]) # born individuals each year
  sim$parentsRepro <- rep(list(sim$lynx[0, ]), times(sim)$end[1]) # reproducing parents each year
  sim$deadLynxColl <- rep(list(sim$lynx[0, ]), times(sim)$end[1]) # dead lynx by collisions each year
  sim$deadLynxNoColl <- rep(list(sim$lynx[0, ]), times(sim)$end[1]) # dead lynx other than by collision each year
  sim$deadOldLynx <- rep(list(sim$lynx[0, ]), times(sim)$end[1]) # dead lynx because of old age each year
  sim$resLynx <- rep(list(sim$lynx[0, ]), times(sim)$end[1]) # individuals becoming resident each year
  sim$timRes <- data.frame(who = numeric(), year = numeric(), time = numeric()) # at which time individuals became resident
  sim$deadDisp <- data.frame(nDisp = numeric(), nDispDeadColl = numeric(), nDispDeadDaily = numeric(), time = numeric()) # how many lynx died during dispersal
  # Individuals that will disperse the following year
  sim$dispOfTheYear <- NLwith(agents = sim$lynx, var = "status", val = "disp")
  # Females that can reproduce 
  sim$FemRes <- rep(list(sim$lynx[0, ]), times(sim)$end[1])
  sim$nKittyBorn <- list()
  sim$dailyDist <- data.frame(dailyDist = numeric(), year = numeric())
  sim$occHab <- data.frame(occHab = numeric(), year = numeric())
  
  
  return(invisible(sim))
}

### Reproduction
reproduction <- function(sim) {

  # Resident females with associated resident males can reproduce
  resInd <- NLwith(agents = sim$lynx, var = "status", val = "res")
  sim$FemRes[[time(sim, "year")[1]]] <- NLwith(agents = resInd, var = "sex", val = "F")
  infoLynx <- of(agents = resInd, var =  c("who", "age", "maleID"))
  # infoLynx <- sim$resInd[[time(sim, "year")[1]]]@.Data[, c("who", "age", "maleID"), drop = FALSE]
  # Adding regarding S?ugetierkunde 1991 publication on sexual maturation in boreal lynx
  # Males of 1 3/4 (i.e., 2 years old) are 50% fertile, they are all fertile at 2 3/4 (i.e., 3 years old)
  # Females of 3/4 are 50% fertile (i.e., 1 year old), they are all fertile at 1 3/4 (i.e., 2 years old)
  # We add to the potential parents, males of 2 years old and females of 1 year old with a 0.5 probability
  maleReproID <- infoLynx[infoLynx[, "age"] >= P(sim)$minAgeReproM, "who"] # resident males in age of reproduction associated to females
  males2yrs <- infoLynx[infoLynx[, "age"] == 2, "who"]
  maleReproID <- c(maleReproID, males2yrs[which(rbinom(n = length(males2yrs), size = 1, prob = 0.5) == 1)])

  femReproID <- infoLynx[infoLynx[, "age"] >= P(sim)$minAgeReproF & infoLynx[, "maleID"] %in% maleReproID, "who"]
  females1yr <- infoLynx[infoLynx[, "age"] == 1 & infoLynx[, "maleID"] %in% maleReproID, "who"]
  femReproID <- c(femReproID, females1yr[which(rbinom(n = length(females1yr), size = 1, prob = 0.5) == 1)])
  
  toRepro <- rbinom(length(femReproID), size = 1, prob = P(sim)$pRepro)
  momsID <- femReproID[toRepro == 1] # ID of the females that will reproduce

  if(length(momsID) != 0) {
    # Number of kittens each female will have depending of their age
    ageFemales <- of(agents = turtle(turtles = sim$lynx, who = momsID), var = "age")
    nKittyBornYoungF <- sample(x = P(sim)$nKittyYoungF, size = length(which(ageFemales <= P(sim)$maxAgeYoungF)),
                               prob = P(sim)$pKittyYoungF, replace = TRUE)
    nKittyBornOldF <- sample(x = P(sim)$nKittyOldF, size = length(which(ageFemales > P(sim)$maxAgeYoungF)),
                             prob = P(sim)$pKittyOldF, replace = TRUE)
    nKittyBorn <- numeric(length(ageFemales))
    nKittyBorn[ageFemales <= P(sim)$maxAgeYoungF] <- nKittyBornYoungF
    nKittyBorn[ageFemales > P(sim)$maxAgeYoungF] <- nKittyBornOldF
    sim$nKittyBorn[[time(sim, "year")[1]]] <- nKittyBorn
    
    # Test
    if(P(sim)$testON == TRUE) {
      expect_equivalent(length(momsID), length(nKittyBorn))
      NpopBeforeRepro <- NLcount(sim$lynx)
    }
    
    if(sum(nKittyBorn) != 0){
      
      # Create the kitten
      sim$lynx <- hatch(turtles = sim$lynx, who = momsID, n = nKittyBorn, breed = "kitty")
      # Update the kitten variables
      kitten <- NLwith(agents = sim$lynx, var = "breed", val = "kitty")
      sexKitten <- sample(c("F", "M"), size = NLcount(kitten), replace = TRUE)
      colorKitten <- randomColor(count = NLcount(kitten), hue = "random", luminosity = "random")
      kittenVar <- cbind.data.frame(prevX = NA,
                                    prevY = NA,
                                    breed = "turtle",
                                    color = colorKitten,
                                    sex = sexKitten,
                                    age = 0,
                                    status = "kitty",
                                    steps = 0,
                                    # lastDispX = NA, # to avoid bug, newborn inherit as the last known good habitat the one from their mother
                                    # lastDispY = NA,
                                    nMat = 0,
                                    maleID = NA,
                                    nFem = 0,
                                    rdMortTerr = 0)
      sim$lynx <- NLset(turtles = sim$lynx, agents = kitten, var = colnames(kittenVar), val = kittenVar)
      # Assign the population name of the newborns where there are
      popHere <- of(world = sim$popDist, agents = patchHere(world = sim$popDist, turtles = kitten))
      popHereName <- rep("Unknown", length(popHere))
      popHereName[popHere == 1] <-  "Alps"
      popHereName[popHere == 2] <-  "Jura"
      popHereName[popHere == 3] <-  "Vosges-Palatinate"
      popHereName[popHere == 4] <-  "BlackForest"
      sim$lynx <- NLset(turtles = sim$lynx, agents = kitten, var = "pop", val = popHereName)
      sim$bornLynx[[time(sim, "year")[1]]] <- turtle(turtles = sim$lynx, who = of(agents = kitten, var = "who")) # newborns of the year
      sim$parentsRepro[[time(sim, "year")[1]]] <- turtle(turtles = sim$lynx,
                                                         who = unique(c(momsID, of(agents = turtle(turtles = sim$lynx, who = momsID), var = "maleID")))) # reproducing individuals of the year
      
      # Test
      if(P(sim)$testON == TRUE) {
        expect_equivalent(NpopBeforeRepro + NLcount(kitten), NLcount(sim$lynx))
        cellKitten <- patchHere(world = sim$habitatMap,
                                turtles = NLwith(agents = sim$lynx, var = "status", val = "kitty"))
        if(NROW(cellKitten) != 0) {
          femWithKitten <- turtlesOn(world = sim$habitatMap,
                                     turtles = NLwith(agents =
                                                        NLwith(agents = sim$lynx, var = "status", val = "res"),
                                                      var = "sex", val = "F"), agents = cellKitten)
          expect_equivalent(sum(is.na(of(agents = femWithKitten, var = "maleID"))), 0)
        }
      }
      
    } # end of if(sum(nKittyBorn) != 0)
  }
  
  return(invisible(sim))
}

### Annual mortality
mortality <- function(sim) {

  # Need to remove the dispersing of the year
  resident <- NLwith(agents = other(agents = sim$lynx, except = sim$dispOfTheYear),
                     var = "status", val = "res")
  oldLynxID <- numeric()
  deathResID <- numeric()
  deathResRdID <- numeric()
  
  if(NLcount(resident) != 0) {

    # Lynx cannot be older than ageMax
    oldLynx <- NLwith(agents = resident, var = "age", val = P(sim)$ageMax)
    oldLynxID <- of(agents = oldLynx, var = "who")
    
    # Remaining residents
    resident <- other(agents = resident, except = oldLynx)
    if(NLcount(resident) != 0) {
    
    # Residents dying from annual fixed mortality
    # Fixed mortality for residents different regarding their population (when data is available)
    # Population = where they are (not where they are from)
    popResident <- of(world = sim$popDist, agents = patchHere(world = sim$popDist, turtles = resident))
    deathResAlps <- rbinom(n = length(popResident[popResident == 1]),
                           size = 1, prob = P(sim)$pMortResAlps)
    deathResJura <- rbinom(n = length(popResident[popResident == 2]),
                           size = 1, prob = P(sim)$pMortResJura)
    deathResVosges <- rbinom(n = length(popResident[popResident == 3]),
                             size = 1, prob = P(sim)$pMortResVosgesPalatinate)
    deathResBlackForest <- rbinom(n = length(popResident[popResident == 4]),
                                  size = 1, prob = P(sim)$pMortResBlackForest)
    deathRes <- rep(NA, length(popResident))
    deathRes[popResident == 1] <- deathResAlps
    deathRes[popResident == 2] <- deathResJura
    deathRes[popResident == 3] <- deathResVosges
    deathRes[popResident == 4] <- deathResBlackForest

    # Remaining alive resident
    deathResID <- of(agents = resident, var = "who")[deathRes == 1]
    resident <- other(agents = resident, except = turtle(turtles = resident, who = deathResID))
    if(NLcount(resident) != 0) {
    
    # Residents dying from spatial mortality dependent on the road density in their territory
    infoRes <- resident@.Data[, c("who", "maleID", "rdMortTerr"), drop = FALSE]
    # For males => mean value of the female territories they occupy
    sexRes <- of(agents = resident, var = "sex")
    if(sum(sexRes == "M") != 0) {
      # Identify the females paired with male lynx
      femOfMales <- NLwith(agents = sim$lynx, var = "maleID", val = infoRes[sexRes == "M", "who"])
      meanRdMortTerrMales <- aggregate(of(agents = femOfMales, var = "rdMortTerr"),
                                       list(of(agents = femOfMales, var = "maleID")), mean)
      infoRes[infoRes[, "who"] %in% meanRdMortTerrMales[,1], "rdMortTerr"] <- meanRdMortTerrMales[, "x"]
    }
    # Add the correction factor for the residents
    probCollRes <- (infoRes[, "rdMortTerr"]) * P(sim)$corrFactorRes
    probCollRes <- ifelse(probCollRes > 1, 1, probCollRes)
    deathResRd <- rbinom(n = NLcount(resident), size = 1, prob = probCollRes)
    sim$nColl <- rbind(sim$nColl, data.frame(ncoll = sum(deathResRd), time = floor(time(sim))[1]))
    deathResRdID <- of(agents = resident, var = "who")[deathResRd == 1]
    
    }
    }
    
    # Resident individuals that will die
    deadWhoRes <- c(oldLynxID, deathResID, deathResRdID)
    # Test
    if(P(sim)$testON == TRUE) {
      # Individuals should not die from two sources of mortality
      expect_equivalent(length(deadWhoRes), length(unique(deadWhoRes)))
    }
    
    # Empty territories belonging to dead females
    cellDeadFem <- NLwith(world = sim$terrMap, agents = patches(sim$terrMap), val = deadWhoRes)
    sim$terrMap <- NLset(world = sim$terrMap, agents = cellDeadFem, val = NA)
    
    # Remove dead females associated to males
    deadRes <- turtle(turtles = sim$lynx, who = deadWhoRes)
    maleIDLooseFem <- deadRes@.Data[, "maleID"]
    maleIDLooseFem <- maleIDLooseFem[!is.na(maleIDLooseFem)]
    if(length(maleIDLooseFem) != 0) {
      maleIDLooseFemTbl <- table(maleIDLooseFem)
      maleIDLooseFemUniq <- as.numeric(names(maleIDLooseFemTbl))
      maleIDLooseFemCnt <- as.numeric(maleIDLooseFemTbl)
      maleLooseFem <- turtle(turtles = sim$lynx, who = maleIDLooseFemUniq)
      newFemCount <- maleLooseFem@.Data[, "nFem"] - maleIDLooseFemCnt
      sim$lynx <- NLset(turtles = sim$lynx, agents = maleLooseFem, var = "nFem", val = newFemCount)
      # If some males end up with 0 female, they become disperser again
      if(NLcount(maleLooseFem[newFemCount == 0]) != 0){
        sim$lynx <- NLset(turtles = sim$lynx, agents = maleLooseFem[newFemCount == 0], var = "status", val = "disp")
      }
    }
    
    # Kill kitten associated to dead females
    femDie <- NLwith(agents = deadRes, var = "sex", val = "F")
    kittenDie <- turtlesOn(world = sim$terrMap,
                           turtles = NLwith(agents = sim$lynx, var = "status", val = "kitty"),
                           agents = femDie, simplify = TRUE)
    kittenDieWho <- kittenDie@.Data[, "who"]
    
    # Remove dead males associated to females
    femaleLooseMale <- NLwith(agents = sim$lynx, var = "maleID", val = deadWhoRes)
    if(NLcount(femaleLooseMale) != 0) {
      sim$lynx <- NLset(turtles = sim$lynx, agents = femaleLooseMale, var = "maleID", val = NA)
    }
    
    # Test
    if(P(sim)$testON == TRUE) {
      countPop <- NLcount(sim$lynx)
    }

    # Remove individuals from the population
    if(length(deathResRdID) != 0){
      sim$deadLynxColl[[time(sim, "year")[1]]] <- turtleSet(sim$deadLynxColl[[time(sim, "year")[1]]], turtle(turtles = sim$lynx, who = deathResRdID)) # add the new lynx dead by collisions
    }
    if(length(deathResID) != 0){
      sim$deadLynxNoColl[[time(sim, "year")[1]]] <- turtleSet(sim$deadLynxNoColl[[time(sim, "year")[1]]], turtle(turtles = sim$lynx, who = deathResID)) # add the new lynx dead other than by collisions
    }
    if(length(oldLynxID) != 0){
      sim$deadOldLynx[[time(sim, "year")[1]]] <- turtleSet(sim$deadOldLynx[[time(sim, "year")[1]]], turtle(turtles = sim$lynx, who = oldLynxID)) # add the new lynx dead of old age
    }
    
    sim$lynx <- die(turtles = sim$lynx, who = c(deadWhoRes, kittenDieWho))
    
    # Test
    if(P(sim)$testON == TRUE) {
      expect_length(intersect(deadWhoRes, kittenDieWho), 0)
      expect_equivalent(NLcount(sim$lynx), countPop - (length(deadWhoRes) + length(kittenDieWho)))
      infoPop <- of(agents = sim$lynx, var = c("who", "maleID", "nFem"))
      expect_length(intersect(deadWhoRes, infoPop[, "who"]), 0)
      expect_true(all(infoPop[,"maleID"][!is.na(infoPop[,"maleID"])] %in% infoPop[,"who"]))
      locKitty <- unique(patchHere(world = sim$terrMap, turtles = NLwith(agents = sim$lynx, var = "status",
                                                                         val = "kitty")))
      lynxRes <- NLwith(agents = sim$lynx, var = "status", val = "res")
      lynxResFem <- turtlesOn(world = sim$terrMap,
                              turtles = NLwith(agents = lynxRes, var = "sex", val = "F"),
                              agents = locKitty, simplify = TRUE)
      expect_true(NROW(locKitty) <= NLcount(lynxResFem))
      expect_equivalent(sum(infoPop[,"nFem"]), length(infoPop[,"maleID"][!is.na(infoPop[,"maleID"])]))
      expect_true(all(of(agents = sim$lynx, var = "nFem") >= 0))
      terrNumTerrMap <- unique(of(world = sim$terrMap, agents = patches(sim$terrMap)))
      terrNumLynx <- of(agents = NLwith(agents = NLwith(agents = sim$lynx, var = "sex", val = "F"),
                                        var = "status", val = "res"), var = "who")
      expect_true(all(terrNumTerrMap[!is.na(terrNumTerrMap)] %in% terrNumLynx))
    }
  }
  
  # With the new order, dispOfTheYear need to be created just before dispersal
  # therefore in the demography submodel
  # Individuals that will disperse this year
  # sim$dispOfTheYear <- NLwith(agents = sim$lynx, var = "status", val = "disp")

  return(invisible(sim))
}

### Dispersal
dispersal <- function(sim) {

  disperser <- NLwith(agents = sim$lynx, var = "status", val = "disp")
  nDisp <- NLcount(disperser)
  nonDisperser <- other(agents = sim$lynx, except = disperser)
  nonDisperserID <- nonDisperser@.Data[, "who"]
  sim$deadDisp <- rbind(sim$deadDisp, data.frame(nDisp = nDisp, nDispDeadColl = 0, nDispDeadDaily = 0, time = floor(time(sim))[1]))
  
  if(nDisp != 0) {
    
    # Number of steps dispersers can walk during the day
    steps <- sample(x = 1:P(sim)$sMaxPs, size = nDisp, prob = sim$Ps, replace = TRUE)
    stepsDisp <- 1:max(steps)
    #disperser <- NLset(turtles = disperser, agents = disperser, var = "steps", val = steps)
    disperser@.Data[, "steps"] <- as.numeric(steps) # faster
    
    for(step in stepsDisp) {
      if(NLcount(disperser) != 0){ # update of the disperser at each "step" loop
        
        dispersingIndNMatMax <- noTurtles()
        
        # Dispersers which have not reach their max number of steps yet
        infoDisp <- of(agents = disperser, var = c("who", "status", "steps"))
        dispersingID <- infoDisp[step <= infoDisp[, "steps"] & infoDisp[, "status"] == "disp", "who"]
        dispersingInd <- turtle(turtles = disperser, who = dispersingID)
        nonDispersingInd <- other(agents = disperser, except = dispersingInd)
        
        # Test
        if(P(sim)$testON == TRUE) {
          infoDispersingInd <- of(agents = dispersingInd, var = c("status", "steps"))
          expect_true(all(infoDispersingInd[, "status"] == "disp"))
          expect_true(all(infoDispersingInd[, "steps"] >= step))
          infoNonDispersingInd <- of(agents = nonDispersingInd, var = c("status", "steps"))
          if(NLcount(nonDispersingInd) != 0){
            expect_true(all(infoNonDispersingInd[, "status"] == "res" |
                              infoNonDispersingInd[, "steps"] < step))
          }
        }
        
        if(NLcount(dispersingInd) != 0){
          
          # Next step in one of the 9 (or less) cells = 8 (or less) neighboring cells + current location
          neighCells <- NetLogoR::neighbors(world = sim$habitatMap, agents = dispersingInd, nNeighbors = 8,
                                            torus = FALSE)
          currCells <- patchHere(world = sim$habitatMap, turtles = dispersingInd)
          currCells <- cbind(currCells, id = 1:NLcount(dispersingInd))
          nextCells <- rbind(neighCells, currCells)
          
          # First, selection of cell type with preference for breeding or dispersal over matrix
          nextCells <- cbind(nextCells,
                             cellType = of(world = sim$habitatMap,
                                           agents = nextCells[, c("pxcor", "pycor")]))
          nextCellsDT <- as.data.table(nextCells)
          cellTypeFreqFull <- nextCellsDT[ , count := .N, by = list(id, cellType)]
          cellTypeFreqFull <- cellTypeFreqFull[, c("id", "cellType", "count")]
          #setkey(cellTypeFreqFull) # not necessary with dplyr::distinct()
          #cellTypeFreq <- as.matrix(unique(cellTypeFreqFull)) # cbind is faster than as.matrix
          #cellTypeFreqUnik <- unique(cellTypeFreqFull)
          cellTypeFreqUnik <- dplyr::distinct(cellTypeFreqFull) # faster than unique
          cellTypeFreq <- cbind(id = cellTypeFreqUnik$id, cellType = cellTypeFreqUnik$cellType,
                                count = cellTypeFreqUnik$count)
          
          # Test
          if(P(sim)$testON == TRUE) {
            expect_equivalent(sum(cellTypeFreq[, "count"]), NROW(nextCells))
          }
          
          withMatrix <- unique(cellTypeFreq[cellTypeFreq[, "cellType"] == 2, "id"])
          withoutMatrix <- unique(cellTypeFreq[!cellTypeFreq[, "id"] %in% withMatrix, "id"])
          pChooseMatrix <- P(sim)$pMat * cellTypeFreq[cellTypeFreq[, "id"] %in% withMatrix &
                                                        cellTypeFreq[, "cellType"] == 2, "count"]
          choseMatrix <- rbinom(n = length(pChooseMatrix), size = 1, prob = pChooseMatrix)
          onlyMat <- unique(cellTypeFreq[, "id"])[
            !unique(cellTypeFreq[, "id"]) %in%
              unique(cellTypeFreq[cellTypeFreq[, "cellType"] %in% c(3, 4) , "id"])]
          nextCellsType <- rbind(nextCells[nextCells[, "cellType"] == 2 &
                                             nextCells[, "id"] %in%
                                             c(withMatrix[choseMatrix == 1], onlyMat), ],
                                 nextCells[nextCells[, "cellType"] %in% c(3, 4) &
                                             nextCells[, "id"] %in% c(withoutMatrix,
                                                                      withMatrix[choseMatrix == 0]), ])
          
          # Second, selection of cell based on the direction
          if(step == 1) { # no correlation
            
            nextCellsTypeDT <- as.data.table(nextCellsType)
            nextCellsTypeDTsampled <- nextCellsTypeDT[nextCellsTypeDT[, .I[sample(.N,1)], by = id]$V1]
            # chosenCells <- as.matrix(nextCellsTypeDTsampled) # the following line is fater than this one
            chosenCells <- cbind(pxcor = nextCellsTypeDTsampled$pxcor, pycor = nextCellsTypeDTsampled$pycor,
                                 id = nextCellsTypeDTsampled$id, cellType = nextCellsTypeDTsampled$cellType)
            chosenCells <- chosenCells[order(chosenCells[, "id"]), , drop = FALSE]
            
            # Test
            if(P(sim)$testON == TRUE) {
              expect_equivalent(NROW(chosenCells), length(unique(chosenCells[, "id"])))
              expect_equivalent(NROW(chosenCells), length(unique(nextCellsType[, "id"])))
            }
            
          } else { # movement correlation
            
            dispCell <- cbind(patchHere(world = sim$habitatMap, turtles = dispersingInd),
                              id = 1:NLcount(dispersingInd))
            colnames(dispCell)[c(1, 2)] <- c("pxcorHere", "pycorHere")
            nextCellsType <- merge(nextCellsType, dispCell)
            nextCellsType <- nextCellsType[order(nextCellsType[, "id"]), , drop = FALSE]
            
            # Individuals with a correlated movement
            probCorr <- rbinom(n = NLcount(dispersingInd), size = 1, prob = P(sim)$pCorr)
            
            # Individuals without a correlated movement
            noCorr <- nextCellsType[nextCellsType[, "id"] %in%
                                      unique(nextCellsType[, "id"])[probCorr == 0], ]
            chosenCellsNoCorr <- cbind(pxcor = numeric(), pycor = numeric(),  id = numeric(),
                                       cellType = numeric())
            if(length(noCorr) != 0) {
              noCorrDT <- as.data.table(noCorr)
              # chosenCellsNoCorr <- as.matrix(noCorrDT[noCorrDT[, .I[sample(.N,1)], by = id]$V1]) # the following two lines are faster than this single one line
              chosenCellsNoCorrDT <- noCorrDT[noCorrDT[, .I[sample(.N,1)], by = id]$V1]
              chosenCellsNoCorr <- cbind(id = chosenCellsNoCorrDT$id, pxcor = chosenCellsNoCorrDT$pxcor,
                                         pycor = chosenCellsNoCorrDT$pycor, cellType = chosenCellsNoCorrDT$cellType,
                                         pxcorHere = chosenCellsNoCorrDT$pxcorHere, pycorHere = chosenCellsNoCorrDT$pycorHere)
              chosenCellsNoCorr <- chosenCellsNoCorr[, c("pxcor", "pycor", "id", "cellType")]
            }
            
            # Individuals with a correlated movement
            yesCorr <- unique(nextCellsType[, "id"])[probCorr == 1]
            chosenCellsYesCorrSelect <- cbind(pxcor = numeric(), pycor = numeric(),  id = numeric(),
                                              cellType = numeric())
            if(length(yesCorr) != 0) {
              nextCellsTypeDir <- cbind(nextCellsType[nextCellsType[, "id"] %in% yesCorr, ],
                                        dir = as.numeric(NA))
              for(eachYesCorr in yesCorr) {
                indYessCorr <- turtle(turtles = dispersingInd, who = dispersingID[eachYesCorr])
                dirCells <- towards(agents = indYessCorr, agents2 = cbind(
                  pxcor = nextCellsTypeDir[nextCellsTypeDir[, "id"] == eachYesCorr, "pxcor"],
                  pycor = nextCellsTypeDir[nextCellsTypeDir[, "id"] == eachYesCorr, "pycor"]),
                  torus = FALSE)
                nextCellsTypeDir[nextCellsTypeDir[, "id"] == eachYesCorr, "dir"] <-
                  round(subHeadings(angle1 = indYessCorr@.Data[, "heading"], angle2 = dirCells,
                                    range360 = TRUE))
                
              }
              
              # Test
              if(P(sim)$testON == TRUE) {
                expect_true(all(nextCellsTypeDir[, "dir"] %in% c(0, 45, 90, 135, 180, 225, 270, 315)))
                expect_equivalent(length(yesCorr), length(unique(nextCellsTypeDir$id)))
              }
              
              # Rank the direction as preferences
              nextCellsTypeDir <- cbind(nextCellsTypeDir, prefDir = 1)
              nextCellsTypeDir[nextCellsTypeDir[, "dir"] %in% c(45, 315), "prefDir"] <- 2
              nextCellsTypeDir[nextCellsTypeDir[, "dir"] %in% c(90, 270), "prefDir"] <- 3
              nextCellsTypeDir[nextCellsTypeDir[, "dir"] %in% c(135, 225), "prefDir"] <- 4
              nextCellsTypeDir[nextCellsTypeDir[, "dir"] == 180, "prefDir"] <- 5
              nextCellsTypeDir[nextCellsTypeDir[, "pxcor"] == nextCellsTypeDir[, "pxcorHere"]
                               & nextCellsTypeDir[, "pycor"] == nextCellsTypeDir[, "pycorHere"],
                               "prefDir"] <- 3
              nextCellsTypeDirDT <- as.data.table(nextCellsTypeDir)
              chosenCellsYesCorrDT <- nextCellsTypeDirDT[nextCellsTypeDirDT[, .I[sample(.N,1)],
                                                                            by = c("id","prefDir")]$V1]
              # Select the cell with the smallest rotation
              #chosenCellsYesCorrSelect <- as.matrix(chosenCellsYesCorrDT[, .SD[which.min(prefDir)], by = id]) # the two lines after are faster than these two lines
              #chosenCellsYesCorrSelect <- chosenCellsYesCorrSelect[, c("pxcor", "pycor", "id", "cellType")]
              chosenCellsYesCorrSelectDT <- chosenCellsYesCorrDT[, .SD[which.min(prefDir)], by = id]
              chosenCellsYesCorrSelect <- cbind(pxcor = chosenCellsYesCorrSelectDT$pxcor, pycor = chosenCellsYesCorrSelectDT$pycor,
                                                id = chosenCellsYesCorrSelectDT$id, cellType = chosenCellsYesCorrSelectDT$cellType)
            }
            
            # Regroup the individuals that had not a correlation movement and the ones that had
            chosenCells <- rbind(chosenCellsNoCorr, chosenCellsYesCorrSelect)
            chosenCells <- chosenCells[order(chosenCells[, "id"]), , drop = FALSE]
            
            # Test
            if(P(sim)$testON == TRUE) {
              expect_equivalent(NROW(chosenCells), length(unique(chosenCells[, "id"])))
              expect_equivalent(NROW(chosenCells), length(unique(nextCellsType[, "id"])))
              expect_equivalent(NROW(chosenCells), NLcount(dispersingInd))
            }
          }
          
          # Lynx memory
          chosenMat <- chosenCells[chosenCells[, "cellType"] == 2, , drop = FALSE]
          if(NROW(chosenMat) != 0) {
            dispersingIndMat <- turtle(turtles = dispersingInd, who = dispersingID[chosenMat[, "id"]])
            dispersingIndMatnSteps <- dispersingIndMat@.Data[, "nMat"]
            #dispersingInd <- NLset(turtles = dispersingInd, agents = dispersingIndMat, var = "nMat",
            #                       val = dispersingIndMatnSteps + 1)
            dispersingInd@.Data[match(dispersingIndMat@.Data[, "who"], dispersingInd@.Data[, "who"]), "nMat"] <- as.numeric(dispersingIndMatnSteps + 1) # faster
            
            # Use memory to find a dispersal cell
            if(sum(dispersingIndMatnSteps + 1 == P(sim)$nMatMax) != 0) {
              chosenCells <- as.data.frame(chosenCells)
              chosenCells[chosenCells[, "cellType"] == 2, c("pxcor", "pycor")][
                dispersingIndMatnSteps + 1 == P(sim)$nMatMax, ] <- dispersingIndMat[
                  dispersingIndMatnSteps + 1 == P(sim)$nMatMax]@.Data[, c("lastDispX", "lastDispY")]
              
              # Test
              if(P(sim)$testON == TRUE) {
                expect_equivalent(of(agents = NLwith(agents = dispersingInd, var = "nMat", val = P(sim)$nMatMax),
                                     var = "who"),
                                  of(agents = dispersingIndMat[dispersingIndMatnSteps + 1 == P(sim)$nMatMax],
                                     var = "who"))
                expect_equivalent(of(agents = NLwith(agents = dispersingInd, var = "nMat", val = P(sim)$nMatMax),
                                     var = c("lastDispX", "lastDispY")),
                                  dispersingIndMat[dispersingIndMatnSteps + 1 == P(sim)$nMatMax]@.Data[
                                    , c("lastDispX", "lastDispY")])
              }
              
              # Reset nMat
              dispersingIndNMatMax <- NLwith(agents = dispersingInd, var = "nMat", val = P(sim)$nMatMax)
              #dispersingInd <- NLset(turtles = dispersingInd, agents = dispersingIndNMatMax, var = "nMat",
              #                       val = 0)
              dispersingInd@.Data[match(dispersingIndNMatMax@.Data[, "who"], dispersingInd@.Data[, "who"]), "nMat"] <- as.numeric(0) # faster
              
              # Test
              if(P(sim)$testON == TRUE) {
                lastDispCell <- of(agents = NLwith(agents = dispersingInd, var = "nMat", val = P(sim)$nMatMax),
                                   var = c("lastDispX", "lastDispY"))
                expect_equivalent(sum(is.na(lastDispCell)), 0)
                expect_true(all(of(agents = dispersingInd, var = "nMat") < P(sim)$nMatMax))
              }
            }
          }
          
          # Update "lastDispX" and "lastDispY"
          chosenDisp <- chosenCells[chosenCells[, "cellType"] %in% c(4, 3), , drop = FALSE]
          
          # Test
          if(P(sim)$testON == TRUE) {
            expect_equivalent(NROW(chosenMat) + NROW(chosenDisp), length(unique(chosenCells[, "id"])))
          }
          
          if(NROW(chosenDisp) != 0) {
            dispersingIndDisp <- turtle(turtles = dispersingInd,
                                        who = dispersingID[chosenDisp[, "id"]])
            dispersingInd <- NLset(turtles = dispersingInd, agents = dispersingIndDisp,
                                   var = c("lastDispX", "lastDispY", "nMat"),
                                   val = cbind(lastDispX = chosenDisp[, "pxcor"],
                                               lastDispY = chosenDisp[, "pycor"],
                                               nMat = 0))
          }
          
          # Movement
          chosenCellsCoords <- cbind(pxcor = chosenCells[, "pxcor"], pycor = chosenCells[, "pycor"])
          dispersingInd <- face(turtles = dispersingInd, agents2 = chosenCellsCoords)
          # Individuals that went back to their last known dispersal cell
          if(NLcount(dispersingIndNMatMax) != 0){
            dispersingIndNMatMaxHead <- of(agents = dispersingIndNMatMax, var = "heading")
            headChoice <- sapply(dispersingIndNMatMaxHead,
                                 function(x) {
                                   which.min(abs(c(0, 45, 90, 135, 180, 225, 270, 315) - x))
                                 })
            #dispersingInd <- NLset(turtles = dispersingInd, agents = dispersingIndNMatMax,
            #                       var = "heading", val = c(0, 45, 90, 135, 180, 225, 270, 315)[headChoice])
            dispersingInd@.Data[match(dispersingIndNMatMax@.Data[, "who"],
                                      dispersingInd@.Data[, "who"]), "heading"] <- as.numeric(c(0, 45, 90, 135, 180, 225, 270, 315)[headChoice]) # faster
            
            # Reset dispersingIndNMatMax
            dispersingIndNMatMax <- noTurtles()
          }
          dispersingInd <- moveTo(turtles = dispersingInd, agents = chosenCellsCoords)
          # chosenCellsCoords are the dispersers new locations
          # Put + 1 on these cells because dispersers stepped on them
          sim$connectivityMap <- NLset(world = sim$connectivityMap, agents = chosenCellsCoords,
                                       val = of(world = sim$connectivityMap, agents = chosenCellsCoords) + 1)
          
          # Test
          if(P(sim)$testON == TRUE) {
            expect_equivalent(chosenCellsCoords, patchHere(world = sim$habitatMap,
                                                           turtles = dispersingInd))
            expect_equivalent(currCells[, c("pxcor", "pycor")], of(agents = dispersingInd,
                                                                   var = c("prevX", "prevY")))
            expect_equivalent(patchHere(world = sim$habitatMap, turtles = dispersingInd)
                              [chosenCells[, "cellType"] %in% c(3, 4), ],
                              of(agents = dispersingInd, var = c("lastDispX", "lastDispY"))
                              [chosenCells[, "cellType"] %in% c(3, 4), ])
          }
          
          # Spatial mortality influenced by roads
          roadMort <- of(world = sim$roadMortMap, agents = chosenCellsCoords)
          # Add the correction factor for the dispersers
          deathRoad <- rbinom(n = length(roadMort), size = 1, prob = (roadMort / P(sim)$corrFactorDisp))
          # Do not kill the dispersers the first year of simulation
          # because all individuals from the initial population are dispersers
          ## INITIAL MORTALITY
          if(floor(time(sim))[1] == start(sim, "year")[1]){
              deathRoad <- rep(0, length(roadMort))
          }

          sim$nColl <- rbind(sim$nColl, data.frame(ncoll = sum(deathRoad), time = floor(time(sim))[1]))
          deathRoad[roadMort == 1] <- 1 # mortality of 1 on the borders (roadMort == 1) needs to be forced
          deadWhoRoad <- dispersingID[deathRoad == 1]
          if(length(deadWhoRoad) != 0){
            sim$deadLynxColl[[time(sim, "year")[1]]] <- turtleSet(sim$deadLynxColl[[time(sim, "year")[1]]], turtle(turtles = dispersingInd, who = deadWhoRoad)) # add the new lynx dead by collisions
          }
          dispersingInd <- die(turtles = dispersingInd, who = deadWhoRoad)
          sim$aliveDispersingIndID <- dispersingInd@.Data[, "who"]
          sim$deadDisp[sim$deadDisp$time == floor(time(sim))[1], "nDispDeadColl"] <- sim$deadDisp[sim$deadDisp$time == floor(time(sim))[1], "nDispDeadColl"] + length(deadWhoRoad)
          
          # Territory search
          disperser <- turtleSet(dispersingInd, nonDispersingInd)
          disperser <- sortOn(agents = disperser, var = "who")
          disperserID <- disperser@.Data[, "who"]
          sim$lynx <- turtleSet(disperser, nonDisperser)
          sim$lynx <- sortOn(agents = sim$lynx, var = "who")
          sim <- searchTerritory(sim)
          disperser <- turtle(turtles = sim$lynx, who = disperserID)
          nonDisperser <- turtle(turtles = sim$lynx, who = nonDisperserID)

        } # end of if(NLcount(dispersingInd) != 0){
        
      } # end of if(NLcount(disperser) != 0)
    } # end of number of steps during the day
  } # end of if(nDisp != 0)
    
  # Fixed daily mortality for all individuals that were dispersers at the beginning of the year
  # and which are still alive
  # and spatial mortality for individuals that were dispersers at the beginning of the year
  # but are not dispersing anymore (found a territory during the year)
  # and which are still alive
  if((NLcount(sim$lynx) != 0) & (NLcount(sim$dispOfTheYear) != 0) & floor(time(sim))[1] != start(sim, "year")[1]){
    
    allDisp <- turtle(turtles = sim$lynx, who = intersect(of(agents = sim$lynx, var = "who"),
                                                          of(agents = sim$dispOfTheYear, var = "who")))

    if(NLcount(allDisp) != 0){
      
      # Fixed daily mortality for all individuals that were dispersers at the beginning of the year
      # and which are still alive
      
      allDispID <- allDisp@.Data[, "who"]
    
      # Fixed mortality for dispersers different regarding their population (when data is available)
      # Population = where they are (not where they are from)
      popDisperser <- of(world = sim$popDist, agents = patchHere(world = sim$popDist,  turtles = allDisp))
      deathDispAlps <- rbinom(n = length(popDisperser[popDisperser == 1]),
                              size = 1, prob = P(sim)$pMortDispAlps)
      deathDispJura <- rbinom(n = length(popDisperser[popDisperser == 2]),
                              size = 1, prob = P(sim)$pMortDispJura)
      deathDispVosges <- rbinom(n = length(popDisperser[popDisperser == 3]),
                                size = 1, prob = P(sim)$pMortDispVosgesPalatinate)
      deathDispBlackForest <- rbinom(n = length(popDisperser[popDisperser == 4]),
                                     size = 1, prob = P(sim)$pMortDispBlackForest)
      deathDaily <- rep(NA, length(popDisperser))
      deathDaily[popDisperser == 1] <- deathDispAlps
      deathDaily[popDisperser == 2] <- deathDispJura
      deathDaily[popDisperser == 3] <- deathDispVosges
      deathDaily[popDisperser == 4] <- deathDispBlackForest

      # Do not kill the dispersers the first year of simulation
      # because all individuals from the initial population are dispersers
      ## INITIAL MORTALITY
      if(floor(time(sim))[1] == start(sim, "year")[1]){
        deathRoad <- rep(0, length(roadMort))
      }
      
      if(floor(time(sim))[1] == start(sim, "year")[1]){
        deathDaily <- rep(0, length(allDispID))
      }

      deadWhoDaily <- allDispID[deathDaily == 1]
      # Some of the dispersers may have become resident during the year
      # Update variables of dependent individuals
      statusDeadDisp <- of(agents = allDisp, var = "status")
      if(sum(statusDeadDisp == "res") != 0) {

        # Territories of dead females
        cellDeadFem <- NLwith(world = sim$terrMap, agents = patches(sim$terrMap), val = deadWhoDaily)
        sim$terrMap <- NLset(world = sim$terrMap, agents = cellDeadFem, val = NA)
        
        # Males associated to dead females
        deadRes <- turtle(turtles = allDisp, who = deadWhoDaily)
        maleIDLooseFem <- deadRes@.Data[, "maleID"]
        maleIDLooseFem <- maleIDLooseFem[!is.na(maleIDLooseFem)]
        if(length(maleIDLooseFem) != 0) {
          maleIDLooseFemTbl <- table(maleIDLooseFem)
          maleIDLooseFemUniq <- as.numeric(names(maleIDLooseFemTbl))
          maleIDLooseFemCnt <- as.numeric(maleIDLooseFemTbl)
          maleLooseFem <- turtle(turtles = sim$lynx, who = maleIDLooseFemUniq)
          newFemCount <- maleLooseFem@.Data[, "nFem"] - maleIDLooseFemCnt
          sim$lynx <- NLset(turtles = sim$lynx, agents = maleLooseFem, var = "nFem", val = newFemCount)
          if(NLcount(maleLooseFem[newFemCount == 0]) != 0){
            sim$lynx <- NLset(turtles = sim$lynx, agents = maleLooseFem[newFemCount == 0], var = "status", val = "disp")
            # Update also allDisp because reused in the spatial mortality to know which are dispersers and residents
            allDisp <- NLset(turtles = allDisp, agents = maleLooseFem[newFemCount == 0], var = "status", val = "disp")
          }
        }

        # Females associated to dead males
        femaleLooseMale <- NLwith(agents = sim$lynx, var = "maleID", val = deadWhoDaily)
        if(NLcount(femaleLooseMale) != 0) {
          sim$lynx <- NLset(turtles = sim$lynx, agents = femaleLooseMale, var = "maleID", val = NA)
        }
      }
      
      # Test
      if(P(sim)$testON == TRUE) {
        countPop <- NLcount(sim$lynx)
        countAllDisp <- NLcount(allDisp)
      }

      # Kill the individuals
      if(length(deadWhoDaily) != 0){
        # These dead individuals should be counted as "dispersers" as they die during their dispersal year
        # However they obtain a resident status so need to change their status
        allDisp <- NLset(turtles = allDisp, agents = turtle(turtles = allDisp, who = deadWhoDaily), var = "status", val = "disp")
        sim$deadLynxNoColl[[time(sim, "year")[1]]] <- turtleSet(sim$deadLynxNoColl[[time(sim, "year")[1]]], turtle(turtles = allDisp, who = deadWhoDaily)) # add the new lynx dead by other than by collisions
      }
      sim$lynx <- die(turtles = sim$lynx, who = deadWhoDaily)
      sim$deadDisp[sim$deadDisp$time == floor(time(sim))[1], "nDispDeadDaily"] <- length(deadWhoDaily)
      allDisp <- die(turtles = allDisp, who = deadWhoDaily) # for later (allDisp is reused later)

      # Test
      if(P(sim)$testON == TRUE) {
        expect_equivalent(NLcount(sim$lynx), countPop - length(deadWhoDaily))
        expect_equivalent(NLcount(allDisp), countAllDisp - length(deadWhoDaily))
      }
      
      sim$lynx <- sortOn(agents = sim$lynx, var = "who")

      # Test
      if(P(sim)$testON == TRUE) {
        infoPop <- of(agents = sim$lynx, var = c("who", "maleID", "nFem"))
        expect_length(intersect(deadWhoDaily, infoPop[, "who"]), 0)
        expect_true(all(infoPop[,"maleID"][!is.na(infoPop[,"maleID"])] %in% infoPop[,"who"]))
        expect_equivalent(sum(infoPop[,"nFem"]), length(infoPop[,"maleID"][!is.na(infoPop[,"maleID"])]))
        expect_true(all(of(agents = sim$lynx, var = "nFem") >= 0))
        terrNumTerrMap <- unique(of(world = sim$terrMap, agents = patches(sim$terrMap)))
        terrNumLynx <- of(agents = NLwith(agents = NLwith(agents = sim$lynx, var = "sex", val = "F"),
                                          var = "status", val = "res"), var = "who")
        expect_true(all(terrNumTerrMap[!is.na(terrNumTerrMap)] %in% terrNumLynx))
      }

      # Spatial mortality for individuals that were dispersers at the beginning of the year
      # but are not dispersing anymore (found a territory during the year)
      # and which are still alive
      formerDisp <- other(agents = NLwith(agents = allDisp, var = "status", val = "res"), 
                          except = disperser) # do not include the current disperser of the day
      # The warning happening here indicate that some of the disperser (except) are not included
      # in the agents we try to exclude them. It's okay, we only want the difference between the two

      if(NLcount(formerDisp) != 0){

        ##########################################################################
        # This block of code is the same as the spatial mortality for the resident
        # except for the correction factor
        # Dispersers dying from spatial mortality dependent on the road density in their territory
        infoFormerDisp <- formerDisp@.Data[, c("who", "maleID", "rdMortTerr"), drop = FALSE]
        # For males => mean value of the female territories they occupy
        sexFormerDisp <- of(agents = formerDisp, var = "sex")
        if(sum(sexFormerDisp == "M") != 0) {
          # Identify the females paired with male lynx
          femOfMales <- NLwith(agents = sim$lynx, var = "maleID", val = infoFormerDisp[sexFormerDisp == "M", "who"])
          meanRdMortTerrMales <- aggregate(of(agents = femOfMales, var = "rdMortTerr"),
                                           list(of(agents = femOfMales, var = "maleID")), mean)
          infoFormerDisp[infoFormerDisp[, "who"] %in% meanRdMortTerrMales[,1], "rdMortTerr"] <- meanRdMortTerrMales[, "x"]
        }
        # Add the correction factor for the residents
        probCollFormerDisp <- ((infoFormerDisp[, "rdMortTerr"]) * P(sim)$corrFactorRes) / 365
        deathFormerDispRd <- rbinom(n = NLcount(formerDisp), size = 1, prob = probCollFormerDisp)
        sim$nColl <- rbind(sim$nColl, data.frame(ncoll = sum(deathFormerDispRd), time = floor(time(sim))[1]))
        
        # Dispersers individuals that will die
        deadWhoFormerDisp <- infoFormerDisp[,"who"][deathFormerDispRd == 1]
        
        # Empty territories belonging to dead females
        cellDeadFem <- NLwith(world = sim$terrMap, agents = patches(sim$terrMap), val = deadWhoFormerDisp)
        sim$terrMap <- NLset(world = sim$terrMap, agents = cellDeadFem, val = NA)
        
        # Remove dead females associated to males
        deadFormerDisp <- turtle(turtles = sim$lynx, who = deadWhoFormerDisp)
        maleIDLooseFem <- deadFormerDisp@.Data[, "maleID"]
        maleIDLooseFem <- maleIDLooseFem[!is.na(maleIDLooseFem)]
        if(length(maleIDLooseFem) != 0) {
          maleIDLooseFemTbl <- table(maleIDLooseFem)
          maleIDLooseFemUniq <- as.numeric(names(maleIDLooseFemTbl))
          maleIDLooseFemCnt <- as.numeric(maleIDLooseFemTbl)
          maleLooseFem <- turtle(turtles = sim$lynx, who = maleIDLooseFemUniq)
          newFemCount <- maleLooseFem@.Data[, "nFem"] - maleIDLooseFemCnt
          sim$lynx <- NLset(turtles = sim$lynx, agents = maleLooseFem, var = "nFem", val = newFemCount)
          # If some males end up with 0 female, they become disperser again
          if(NLcount(maleLooseFem[newFemCount == 0]) != 0){
            sim$lynx <- NLset(turtles = sim$lynx, agents = maleLooseFem[newFemCount == 0], var = "status", val = "disp")
          }
        }

        # Remove dead males associated to females
        femaleLooseMale <- NLwith(agents = sim$lynx, var = "maleID", val = deadWhoFormerDisp)
        if(NLcount(femaleLooseMale) != 0) {
          sim$lynx <- NLset(turtles = sim$lynx, agents = femaleLooseMale, var = "maleID", val = NA)
        }
        
        # Test
        if(P(sim)$testON == TRUE) {
          countPop <- NLcount(sim$lynx)
        }
        
        # Remove individuals from the population
        if(length(deadWhoFormerDisp) != 0){
          sim$deadLynxColl[[time(sim, "year")[1]]] <- turtleSet(sim$deadLynxColl[[time(sim, "year")[1]]], deadFormerDisp) # add the new lynx dead by collisions
          sim$lynx <- die(turtles = sim$lynx, who = deadWhoFormerDisp)
        }
        
        # Test
        if(P(sim)$testON == TRUE) {
          expect_equivalent(NLcount(sim$lynx), countPop - (length(deadWhoFormerDisp)))
          infoPop <- of(agents = sim$lynx, var = c("who", "maleID", "nFem"))
          expect_length(intersect(deadWhoFormerDisp, infoPop[, "who"]), 0)
          expect_true(all(infoPop[,"maleID"][!is.na(infoPop[,"maleID"])] %in% infoPop[,"who"]))
          locKitty <- unique(patchHere(world = sim$terrMap, turtles = NLwith(agents = sim$lynx, var = "status",
                                                                             val = "kitty")))
          lynxRes <- NLwith(agents = sim$lynx, var = "status", val = "res")
          lynxResFem <- turtlesOn(world = sim$terrMap,
                                  turtles = NLwith(agents = lynxRes, var = "sex", val = "F"),
                                  agents = locKitty, simplify = TRUE)
          expect_true(NROW(locKitty) <= NLcount(lynxResFem))
          expect_equivalent(sum(infoPop[,"nFem"]), length(infoPop[,"maleID"][!is.na(infoPop[,"maleID"])]))
          expect_true(all(of(agents = sim$lynx, var = "nFem") >= 0))
          terrNumTerrMap <- unique(of(world = sim$terrMap, agents = patches(sim$terrMap)))
          terrNumLynx <- of(agents = NLwith(agents = NLwith(agents = sim$lynx, var = "sex", val = "F"),
                                            var = "status", val = "res"), var = "who")
          expect_true(all(terrNumTerrMap[!is.na(terrNumTerrMap)] %in% terrNumLynx))
        }

      } # end of if(NLcount(formerDisp) != 0)

    }
    
  }
  
  return(invisible(sim))
}

### Territory search
searchTerritory <- function(sim) {

  disp <- turtle(turtles = sim$lynx, who = sim$aliveDispersingIndID)
  if(NLcount(disp) != 0) {

    # Female dispersers
    dispFem <- NLwith(agents = disp, var = "sex", val = "F")
    if(NLcount(dispFem) != 0) {
      dispFemID <- dispFem@.Data[, "who"]
      # Shuffle dispFemID so that it's not always the smallest IDs (i.e., the oldest individuals) that go first
      dispFemID <- dispFemID[sample(length(dispFemID))]

      # # TRY SOMETHING FASTER
      # for(searchingFemID in dispFemID) {
      #   # To build territory = empty cells of breeding type (= 4)
      #   searchingFemCell <- patchHere(world = sim$habitatMap, turtles = turtle(turtles = sim$lynx, who = searchingFemID))
      #   #searchingFemCellType <- of(world = sim$habitatMap, agents = searchingFemCell)
      #   searchingFemCellType <- sim$habitatMap[searchingFemCell[, 1], searchingFemCell[, 2]] # faster
      #   #searchingFemCellAvail <- of(world = sim$terrMap, agents = searchingFemCell)
      #   searchingFemCellAvail <- sim$terrMap[searchingFemCell[, 1], searchingFemCell[, 2]] # faster
      # 
      #   if(searchingFemCellType == 4 & is.na(searchingFemCellAvail)){ # current position
      #     #terrValues <- of(world = sim$terrMap, agents = patches(sim$terrMap))
      #     terrValues <- as.numeric(t(sim$terrMap@.Data)) # faster
      #     availCellsUpdatedRas <- sim$availCellsRas
      #     occupiedCells <- which(!is.na(terrValues))
      #     availCellsUpdatedRas[occupiedCells] <- 0
      # 
      #     # Spread from the female position (loci) to available contiguous cells
      #     # Find on which territory the female is
      #     terrSizeName <- of(world = sim$popDist, agents = patchHere(world = sim$popDist, turtles = turtle(turtles = sim$lynx, who = searchingFemID)))
      #     # And assign the territory size according to her position
      #     if(terrSizeName == 1){
      #       terrSize <- round(P(sim)$coreTerrSizeFAlps)
      #     }
      #     if(terrSizeName == 2){
      #       terrSize <- round(P(sim)$coreTerrSizeFJura)
      #     }
      #     if(terrSizeName == 3){
      #       terrSize <- round(P(sim)$coreTerrSizeFVosgesPalatinate)
      #     }
      #     if(terrSizeName == 4){
      #       terrSize <- round(P(sim)$coreTerrSizeFBlackForest)
      #     }
      #     terrDT <- spread(landscape = availCellsUpdatedRas, directions = 4,
      #                      loci = cellFromPxcorPycor(world = sim$habitatMap,
      #                                                pxcor = searchingFemCell[, 1],
      #                                                pycor = searchingFemCell[, 2]),
      #                      spreadProb = availCellsUpdatedRas, maxSize = terrSize, returnIndices = TRUE,
      #                      quick = TRUE)
      #     terrCells <- unique(terrDT$indices) # cells of the built territory
      #     if(length(terrCells) == terrSize) {
      #       newTerrCells <- PxcorPycorFromCell(world = sim$habitatMap, cellNum = terrCells)
      # 
      #       # Test
      #       if(P(sim)$testON == TRUE) {
      #         expect_true(all(is.na(of(world = sim$terrMap, agents = newTerrCells))))
      #       }
      # 
      #       # Claim the territory
      #       sim$terrMap <- NLset(world = sim$terrMap, agents = newTerrCells, val = searchingFemID)
      #       # Mortality probability in the territory
      #       #probMortRdTerr <- of(world = sim$roadMortMap, agents = newTerrCells)
      #       probMortRdTerr <- sim$roadMortMap[newTerrCells[, 1], newTerrCells[, 2]] # faster
      #       sim$lynx <- NLset(turtles = sim$lynx, agents = turtle(turtles = sim$lynx, who = searchingFemID),
      #                         var = c("status", "rdMortTerr"),
      #                         val = cbind(status = "res", rdMortTerr = mean(probMortRdTerr)))
      #       # Save the data about the new residents
      #       if(length(searchingFemID) != 0){
      #         sim$resLynx[[time(sim, "year")[1]]] <- turtleSet(sim$resLynx[[time(sim, "year")[1]]], turtle(turtles = sim$lynx, who = searchingFemID))
      #         sim$timeRes <- rbind(sim$timeRes, data.frame(who = searchingFemID, year =  time(sim, "year")[1], time = sim$day))
      #       }
      # 
      #       # Male around to claim the female?
      #       # Check for a male on an area equal to the home range size (95 % kernel density) of the males in the population
      #       # Extract the distance to which look for a male as the radius of its home range size
      #       if(terrSizeName == 1){
      #         maxDistMale <- sqrt(P(sim)$terrSizeMAlps/pi)
      #       }
      #       if(terrSizeName == 2){
      #         maxDistMale <- sqrt(P(sim)$terrSizeMJura/pi)
      #       }
      #       if(terrSizeName == 3){
      #         maxDistMale <- sqrt(P(sim)$terrSizeMVosgesPalatinate/pi)
      #       }
      #       if(terrSizeName == 4){
      #         maxDistMale <- sqrt(P(sim)$terrSizeMBlackForest/pi)
      #       }
      #       neighbTerrCells <- NetLogoR::inRadius(agents = turtle(turtles = sim$lynx, who = searchingFemID), radius = maxDistMale,
      #                                             agents2 = patches(sim$habitatMap), world = sim$habitatMap, torus = FALSE)
      # 
      #       #neighbTerrCells <- unique(of(world = sim$terrMap, agents = neighbTerrCells))
      #       neighbTerrCells <- unique(sim$terrMap[neighbTerrCells[, 1], neighbTerrCells[, 2]]) # faster
      #       otherFemTerr <- neighbTerrCells[!is.na(neighbTerrCells) & neighbTerrCells != searchingFemID]
      #       if(length(otherFemTerr) != 0) {
      #         otherFem <- turtle(turtles = sim$lynx, who = otherFemTerr)
      #         infoOtherFem <- otherFem@.Data[, "maleID"]
      #         infoOtherFem <- infoOtherFem[!is.na(infoOtherFem)]
      #         otherMal <- turtle(turtles = sim$lynx, who = infoOtherFem)
      #         infoOtherMal <- otherMal@.Data[, "nFem"]
      # 
      #         if(length(infoOtherFem[infoOtherMal < 3]) != 0){
      #           # Calculate the distances between the new female resident and all the available males
      #           distFemaleMales <- NetLogoR::NLdist(agents = turtle(turtles = sim$lynx, who = searchingFemID),
      #                                               agents2 = turtle(turtles = sim$lynx, who = infoOtherFem[infoOtherMal < 3]),
      #                                               torus = FALSE)
      #           selectedMal <- ifelse(length(infoOtherFem[infoOtherMal < 3]) == 1,
      #                                 infoOtherFem[infoOtherMal < 3],
      #                                 # Select the closest male if there are several available
      #                                 infoOtherFem[infoOtherMal < 3][which.min(distFemaleMales)])
      # 
      #           # Associate the male to the female
      #           sim$lynx <- NLset(turtles = sim$lynx, agents = turtle(turtles = sim$lynx, who = searchingFemID),
      #                             var = "maleID", val = selectedMal)
      #           selectedMalInd <- turtle(turtles = sim$lynx, who = selectedMal)
      #           sim$lynx <- NLset(turtles = sim$lynx, agents = selectedMalInd, var = "nFem",
      #                             val = selectedMalInd@.Data[, "nFem"] + 1)
      #         }
      #       }
      # 
      #     } # end if(length(terrCells) == terrSize)
      #   } # end if(searchingFemCellType == 4 & is.na(searchingFemCellAvail))
      # } # end for(searchingFemID in dispFemID)

      # Identify females on breeding empty patches
      searchingFemCell <- patchHere(world = sim$habitatMap, turtles = turtle(turtles = sim$lynx, who = dispFemID))
      searchingFemCellType <- sim$habitatMap[searchingFemCell[, 1], searchingFemCell[, 2]] # faster
      searchingFemCellAvail <- sim$terrMap[searchingFemCell[, 1], searchingFemCell[, 2]] # faster

      searchingFemCell <- searchingFemCell[searchingFemCellType == 4 & is.na(searchingFemCellAvail), , drop = FALSE]
      dispFemID <- dispFemID[searchingFemCellType == 4 & is.na(searchingFemCellAvail)] # females on breeding empty patches
      # Remove where there are several females on the same patch (keep only one)
      dispFemID <- dispFemID[!duplicated(searchingFemCell)]
      searchingFemCell <- searchingFemCell[!duplicated(searchingFemCell), , drop = FALSE]

      if(length(dispFemID) != 0){
        # Patches available to build a territory (breeding and not already in a territory)
        terrValues <- as.numeric(t(sim$terrMap@.Data)) # faster
        availCellsUpdatedRas <- sim$availCellsRas
        occupiedCells <- which(!is.na(terrValues))
        availCellsUpdatedRas[occupiedCells] <- 0

        # Spread from the female positions (loci) to available contiguous cells
        # Find on which territory the female are
        terrSizeName <- of(world = sim$popDist, agents = patchHere(world = sim$popDist, turtles = turtle(turtles = sim$lynx, who = dispFemID)))
        terrSize <- terrSizeName
        terrSize[which(terrSizeName == 1)] <- round(P(sim)$coreTerrSizeFAlps)
        terrSize[which(terrSizeName == 2)] <- round(P(sim)$coreTerrSizeFJura)
        terrSize[which(terrSizeName == 3)] <- round(P(sim)$coreTerrSizeFVosgesPalatinate)
        terrSize[which(terrSizeName == 4)] <- round(P(sim)$coreTerrSizeFBlackForest)

        terrDT <- spread(landscape = availCellsUpdatedRas, directions = 4,
                         loci = cellFromPxcorPycor(world = sim$habitatMap,
                                                   pxcor = searchingFemCell[, 1],
                                                   pycor = searchingFemCell[, 2]),
                         spreadProb = availCellsUpdatedRas, maxSize = terrSize, returnIndices = TRUE,
                         quick = TRUE , allowOverlap = FALSE)

        # How many unique patch per territory per female
        uniquePatches <- terrDT %>%
          group_by(id) %>%
          summarise(count = n_distinct(indices))

        # Females which can create a territory
        dispFemIDsub <- dispFemID[which(uniquePatches$count >= terrSize)] # ranked in dispFemID order

        if(length(dispFemIDsub) > 1){
          # Now we need to see that territories are not overlapping
          overlappingPatches <- as.numeric(names(table(terrDT$indices)[which(table(terrDT$indices)>1)]))
          if(length(overlappingPatches) != 0){
            for(eachOverPatch in overlappingPatches){
              overlappedFem <- terrDT[terrDT$indices == eachOverPatch, "id"]
              # Remove from dispFemIDsub the females that have overlapped patches but keeping the first one who claim the patch
              dispFemIDsub <- dispFemIDsub[!dispFemIDsub %in% dispFemID[as.numeric(overlappedFem$id[-1])]]
            }
          }
        }

        for(searchingFemID in dispFemIDsub) {

          terrCells <- unique(terrDT[terrDT$id == which(dispFemID == searchingFemID),][[3]]) # cells of the built territory
          newTerrCells <- PxcorPycorFromCell(world = sim$habitatMap, cellNum = terrCells)

          # Test
          if(P(sim)$testON == TRUE) {
            expect_true(all(is.na(of(world = sim$terrMap, agents = newTerrCells))))
          }

          # Claim the territory
          sim$terrMap <- NLset(world = sim$terrMap, agents = newTerrCells, val = searchingFemID)
          # Mortality probability in the territory
          #probMortRdTerr <- of(world = sim$roadMortMap, agents = newTerrCells)
          probMortRdTerr <- sim$roadMortMap[newTerrCells[, 1], newTerrCells[, 2]] # faster
          sim$lynx <- NLset(turtles = sim$lynx, agents = turtle(turtles = sim$lynx, who = searchingFemID),
                            var = c("status", "rdMortTerr"),
                            val = cbind(status = "res", rdMortTerr = mean(probMortRdTerr)))
          # Save the data about the new residents
          sim$resLynx[[time(sim, "year")[1]]] <- turtleSet(sim$resLynx[[time(sim, "year")[1]]], turtle(turtles = sim$lynx, who = searchingFemID))
          sim$timeRes <- rbind(sim$timeRes, data.frame(who = searchingFemID, year =  time(sim, "year")[1], time = sim$day))

          # Male around to claim the female?
          terrSizeName <- of(world = sim$popDist, agents = patchHere(world = sim$popDist, turtles = turtle(turtles = sim$lynx, who = searchingFemID)))
          # Check for a male on an area equal to the home range size (95 % kernel density) of the males in the population
          # Extract the distance to which look for a male as the radius of its home range size
          if(terrSizeName == 1){
            maxDistMale <- sqrt(P(sim)$terrSizeMAlps/pi)
          }
          if(terrSizeName == 2){
            maxDistMale <- sqrt(P(sim)$terrSizeMJura/pi)
          }
          if(terrSizeName == 3){
            maxDistMale <- sqrt(P(sim)$terrSizeMVosgesPalatinate/pi)
          }
          if(terrSizeName == 4){
            maxDistMale <- sqrt(P(sim)$terrSizeMBlackForest/pi)
          }
          neighbTerrCells <- NetLogoR::inRadius(agents = turtle(turtles = sim$lynx, who = searchingFemID), radius = maxDistMale,
                                                agents2 = patches(sim$habitatMap), world = sim$habitatMap, torus = FALSE)

          #neighbTerrCells <- unique(of(world = sim$terrMap, agents = neighbTerrCells))
          neighbTerrCells <- unique(sim$terrMap[neighbTerrCells[, 1], neighbTerrCells[, 2]]) # faster
          otherFemTerr <- neighbTerrCells[!is.na(neighbTerrCells) & neighbTerrCells != searchingFemID]
          if(length(otherFemTerr) != 0) {
            otherFem <- turtle(turtles = sim$lynx, who = otherFemTerr)
            infoOtherFem <- otherFem@.Data[, "maleID"]
            infoOtherFem <- infoOtherFem[!is.na(infoOtherFem)]
            otherMal <- turtle(turtles = sim$lynx, who = infoOtherFem)
            infoOtherMal <- otherMal@.Data[, "nFem"]

            if(length(infoOtherFem[infoOtherMal < 3]) != 0){
              # Calculate the distances between the new female resident and all the available males
              distFemaleMales <- NetLogoR::NLdist(agents = turtle(turtles = sim$lynx, who = searchingFemID),
                                                  agents2 = turtle(turtles = sim$lynx, who = infoOtherFem[infoOtherMal < 3]),
                                                  torus = FALSE)
              selectedMal <- ifelse(length(infoOtherFem[infoOtherMal < 3]) == 1,
                                    infoOtherFem[infoOtherMal < 3],
                                    # Select the closest male if there are several available
                                    infoOtherFem[infoOtherMal < 3][which.min(distFemaleMales)])

              # Associate the male to the female
              sim$lynx <- NLset(turtles = sim$lynx, agents = turtle(turtles = sim$lynx, who = searchingFemID),
                                var = "maleID", val = selectedMal)
              selectedMalInd <- turtle(turtles = sim$lynx, who = selectedMal)
              sim$lynx <- NLset(turtles = sim$lynx, agents = selectedMalInd, var = "nFem",
                                val = selectedMalInd@.Data[, "nFem"] + 1)
            }
          }
        } # for(searchingFemID in dispFemID)
      } # if(length(dispFemID) != 0)
      

    } # end if(NLcount(dispFem) != 0)
    
    # Male dispersers
    dispMal <- NLwith(agents = disp, var = "sex", val = "M")
    if(NLcount(dispMal) != 0) {
      dispMalID <- dispMal@.Data[, "who"]
      
      # Need to find a female territory
      cellMale <- patchHere(world = sim$habitatMap, turtles = dispMal)
      #cellMaleTerr <- of(world = sim$terrMap, agents = cellMale)
      cellMaleTerr <- sim$terrMap[cellMale[, 1], cellMale[, 2]] # faster
      dispMalIDwFem <- dispMalID[!is.na(cellMaleTerr)]
      
      # Shuffle dispMalIDwFem so that it's not always the smallest IDs (i.e., the oldest individuals) that go first
      dispMalIDwFem <- dispMalIDwFem[sample(length(dispMalIDwFem))]
      for(searchingMaleID in dispMalIDwFem) {
        
        whoFemEncountered <- cellMaleTerr[!is.na(cellMaleTerr)][
          dispMalID[!is.na(cellMaleTerr)] == searchingMaleID]
        femEncountered <- turtle(turtles = sim$lynx, who = whoFemEncountered)
        femEncounteredAvail <- femEncountered@.Data[, "maleID"]
        
        if(is.na(femEncounteredAvail)) {
          
          # Claim the female
          sim$lynx <- NLset(turtles = sim$lynx, agents = femEncountered, var = "maleID", val = searchingMaleID)
          maleClaiming <- turtle(turtles = sim$lynx, who = searchingMaleID)
          sim$lynx <- NLset(turtles = sim$lynx, agents = maleClaiming, var = c("status", "nFem"),
                            val = cbind(status =  "res", nFem = 1))
          # Save the data about the new residents
          if(length(maleClaiming) != 0){
            # There can be duplicates here as males becoming resident once can become disperser again if their female(s) die(s)
            # and so when they become resident again afterwards (in the same year) they are duplicated here.
            # However, turtleSet() only keep the first instance so there are no duplicates in the output resLynx, 
            # only the first time will be recorded.
            sim$resLynx[[time(sim, "year")[1]]] <- turtleSet(sim$resLynx[[time(sim, "year")[1]]], turtle(turtles = sim$lynx, who = of(agents = maleClaiming, var = "who")))
            sim$timeRes <- rbind(sim$timeRes, data.frame(who = of(agents = maleClaiming, var = "who"), year =  time(sim, "year")[1], time = sim$day))
          }
          
          # Females around to claim?
          # Check for females on an area equal to the male home range size (95 % kernel density) in the population
          # Extract the distance to which look for a male as the radius of its home range size
          terrSizeName <- of(world = sim$popDist, agents = patchHere(world = sim$popDist, turtles = turtle(turtles = sim$lynx, who = searchingMaleID)))
          if(terrSizeName == 1){
            maxDistMale <- sqrt(P(sim)$terrSizeMAlps/pi)
          }
          if(terrSizeName == 2){
            maxDistMale <- sqrt(P(sim)$terrSizeMJura/pi)
          }
          if(terrSizeName == 3){
            maxDistMale <- sqrt(P(sim)$terrSizeMVosgesPalatinate/pi)
          }
          if(terrSizeName == 4){
            maxDistMale <- sqrt(P(sim)$terrSizeMBlackForest/pi)
          }
          neighbTerrCells <- NetLogoR::inRadius(agents = turtle(turtles = sim$lynx, who = searchingMaleID), radius = maxDistMale,
                                                agents2 = patches(sim$habitatMap), world = sim$habitatMap, torus = FALSE)
          
          neighbTerrCells <- unique(sim$terrMap[neighbTerrCells[, 1], neighbTerrCells[, 2]]) # faster
          otherFemTerr <- neighbTerrCells[!is.na(neighbTerrCells) & neighbTerrCells != whoFemEncountered]
          if(length(otherFemTerr) != 0) {
            otherFem <- turtle(turtles = sim$lynx, who = otherFemTerr)
            infoOtherFem <- otherFem@.Data[, "maleID"]
            if(length(otherFemTerr[is.na(infoOtherFem)]) != 0){
              # Calculate the distances between the new male resident and all the available females 
              distMaleFemales <- NetLogoR::NLdist(agents = turtle(turtles = sim$lynx, who = searchingMaleID),
                                                  agents2 = turtle(turtles = sim$lynx, who = otherFemTerr[is.na(infoOtherFem)]),
                                                  torus = FALSE)
              
              # Select up to 2 females without male
              selectedFem <- ifelse(length(otherFemTerr[is.na(infoOtherFem)]) >= 2,
                                    # Select the closest females if there are more than 2
                                    otherFemTerr[is.na(infoOtherFem)][which.minn(distMaleFemales, n = 2)],
                                    otherFemTerr[is.na(infoOtherFem)])
                                    
                                    
              # Claim the female(s)
              sim$lynx <- NLset(turtles = sim$lynx, agents = turtle(turtles = sim$lynx, who = selectedFem),
                                var = "maleID", val = searchingMaleID)
              sim$lynx <- NLset(turtles = sim$lynx, agents = maleClaiming, var = "nFem",
                                val = 1 + length(selectedFem))
            }
          }
          
        } # end if(is.na(femEncounteredAvail))
      } # end for(searchingMaleID in dispMalIDwFem)
    } # end if(NLcount(dispMal) != 0)
    
    # Test
    if(P(sim)$testON == TRUE) {
      terrNumber <- of(world = sim$terrMap, agents = patches(sim$terrMap))
      terrNumber <- terrNumber[!is.na(terrNumber)]
      expect_true(all(table(terrNumber) >= min(c(P(sim)$coreTerrSizeFAlps, P(sim)$coreTerrSizeFJura, P(sim)$coreTerrSizeFVosgesPalatinate,
                                                 P(sim)$coreTerrSizeFBlackForest))))
      expect_true(all(table(terrNumber) <= max(c(P(sim)$coreTerrSizeFAlps, P(sim)$coreTerrSizeFJura, P(sim)$coreTerrSizeFVosgesPalatinate, 
                                                 P(sim)$coreTerrSizeFBlackForest))))
      infoPop <- sim$lynx@.Data[, c("who", "maleID", "nFem"), drop = FALSE]
      nFemPerMal <- table(infoPop[, "maleID"])
      expect_equivalent(as.numeric(nFemPerMal),
                        infoPop[infoPop[, "who"] %in% as.numeric(names(nFemPerMal)), "nFem"])
      expect_true(all(sim$lynx@.Data[, "nFem"] >= 0 & sim$lynx@.Data[, "nFem"] <= 3))
      terrNumTerrMap <- unique(of(world = sim$terrMap, agents = patches(sim$terrMap)))
      terrNumLynx <- of(agents = NLwith(agents = NLwith(agents = sim$lynx, var = "sex", val = "F"),
                                        var = "status", val = "res"), var = "who")
      expect_true(all(terrNumTerrMap[!is.na(terrNumTerrMap)] %in% terrNumLynx))
    }
  }
  
  return(invisible(sim))
}

### Demography update
demography <- function(sim) {

  # Aging
  ageLynx <- sim$lynx@.Data[, "age"]
  sim$lynx <- NLset(turtles = sim$lynx, agents = sim$lynx, var = "age", val = ageLynx + 1)
  # Kitten become dispersers
  kitten <- NLwith(agents = sim$lynx, var = "status", val = "kitty")
  sim$lynx <- NLset(turtles = sim$lynx, agents = kitten, var = "status", val = "disp")
  
  # Individuals that will disperse the next year
  sim$dispOfTheYear <- NLwith(agents = sim$lynx, var = "status", val = "disp")
  
  # # Individuals that can reproduce (i.e., already resident at the beginning of the year)
  # sim$resInd[[time(sim, "year")[1] + 1]] <- other(agents = sim$lynx, except = sim$dispOfTheYear)
  
  return(invisible(sim))
}

### Plot
plotSim <- function(sim) {

  dev(4)
  Plot(sim$habitatMap, title = "Habitats with lynx territories and positions")
  Plot(sim$terrMap, addTo = "sim$habitatMap", col = "black", zero.color = "transparent", title = "")
  if(NLcount(sim$lynx) != 0){
    Plot(sim$lynx, addTo = "sim$habitatMap", pch = 16, col = of(agents = sim$lynx, var = "color"), title = "")
  }
  
  return(invisible(sim))
}

### Save
saveSimYearly <- function(sim){

  sim$outputLynx[[length(sim$outputLynx) + 1]] <- sim$lynx  
  sim$outputTerrMap[[length(sim$outputTerrMap) + 1]] <- sim$terrMap
  
  return(invisible(sim))
}

saveSimDaily <- function(sim){

  # Daily dispersal distance
  dailyDist <- NLdist(agents = NLwith(agents = sim$tempSaveLynx, var = "who", val = of(agents = sim$lynx, var = "who")), 
                      agents2 = NLwith(agents = sim$lynx, var = "who", val = of(agents = sim$tempSaveLynx, var = "who")))
  if(length(dailyDist) != 0){
    sim$dailyDist <- rbind(sim$dailyDist, data.frame(dailyDist = dailyDist, year =  time(sim, "year")[1]))
  }
  
  # Select dispersal habitat
  occHab <- of(world = sim$habitatMap, agents = patchHere(world = sim$habitatMap, 
                                                          turtles = NLwith(agents = sim$lynx, var = "status", val = "disp")))
  if(length(occHab) != 0){
    sim$occHab <- rbind(sim$occHab, data.frame(occHab = occHab, year =  time(sim, "year")[1]))
  }

  return(invisible(sim))
}
