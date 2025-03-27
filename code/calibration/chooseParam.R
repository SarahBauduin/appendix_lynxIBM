## Phase 1

# Resident mortalities
pMortResAlps <- sample(seq(0.05, 0.25, by = 0.01), size = 2, replace = TRUE)
pMortResJura <- sample(seq(0.05, 0.25, by = 0.01), size = 2, replace = TRUE)
pMortResVosgesPalatinate <- pMortResJura
pMortResBlackForest <- pMortResJura
# Disperser mortalities
pMortDisp_Alps <- sample(seq(0.0001, 0.002, by = 0.0001), size = 2, replace = TRUE)
pMortDisp_Jura <- sample(seq(0.0001, 0.002, by = 0.0001), size = 2, replace = TRUE)
pMortDispVosgesPalatinate <- pMortDispJura
pMortDispBlackForest <- pMortDispJura
# Correction factors for collision probabilities
corrFactorRes <- sample(seq(1, 6, by = 0.5), size = 1)
corrFactorDisp <- sample(seq(50, 300, by = 25), size = 1)


## Phase 2

# Resident mortalities
pMortResAlps <- sample(c(0.13, 0.14, 0.15, 0.16, 0.17), size = 1)
pMortResJura <- sample(seq(from = 0.10, to = 0.21, by = 0.01), size = 1)
pMortResVosgesPalatinate <- pMortResJura
pMortResBlackForest <- pMortResJura
# Disperser mortalities
pMortDispAlps <- sample(seq(from = 0.0005, to = 0.0012, by = 0.0001), size = 1)
pMortDispJura <- sample(seq(from = 0.0005, to = 0.001, by = 0.0001), size = 1)
pMortDispVosgesPalatinate <- pMortDispJura
pMortDispBlackForest <- pMortDispJura
# Correction factors for collision probabilities
corrFactorRes <- sample(seq(from = 3, to = 4, by = 0.1), size = 1)
corrFactorDisp <- sample(seq(from = 125, to = 225, by = 5), size = 1)
