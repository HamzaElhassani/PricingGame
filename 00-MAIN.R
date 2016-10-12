###################################################################################################
######################################## PTF MANIPULATION #########################################
###################################################################################################

# Last Update: 10/07/16
setwd("G:/pricing/")

###################################################################################################
## STEP 00 : LIBRARY                                                                             ##
###################################################################################################

require(dplyr)
require(tidyr)
require(magrittr)
require(FactoMineR)
require(ggplot2)
require(hglm)
require(data.table)
require(stringr)
require(caret)
require(hts)
require(Metrics)
require(ineq)
require(tweedie)
require(statmod)
require(xgboost)
require(randomForest)
library(extraTrees)
setJavaMemory(4000)
library(mlr)


###################################################################################################
## STEP 01 : PROGRAMS                                                                           ##
###################################################################################################

source("00-Functions.R")
source("00-DATA_MANIPULATION.R")
source("01-GLM_MODELING.R")
#source("02-GBM_MODELING.R")
source("03-TWEEDIE_MODELING.R")
source("04-XGBOOST_MODELING.R")
source("05-RF_MODELING.R")
source("06-XT_MODELING.R")

source("99-BLENDING.R")

