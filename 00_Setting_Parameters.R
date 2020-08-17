# ************************************************************ #
#' @title Set parameters for causal-analysis of tenure effects on forest-loss in Brazil
#' @description Definition of base parameters for running a coarsened-exact matching - CEM - algorithm and statistical analysis calculating average effects of tenure 
#' @details {Research question: what is the effect of different tenure types compared to 1. undesignated/untitled "public" and 2. private lands
#'           Methods: matching using CEM + regression + meta-analysis
#'           Data: we used the sqlite format tenure data from imaflora for the tenure data. 
#'            
#' }
#' 
# ************************************************************ #

#------------------------------------------------------------------------#
# libraries needed
#------------------------------------------------------------------------#

library(cem)
library(dplyr)
library(readr)
library(tidyr)
library(sf)
library(foreign)
library(tidyverse)
library(RSQLite)
library(DBI)
library(dbplyr)
library(fastDummies)
library(foreign)
library(rgdal)
library(data.table)
library(ape)
library(sp)
library(BiodiversityR)

#------------------------------------------------------------------------#
# study design
#------------------------------------------------------------------------#
# vector with temporal scales (all small chunks + total)
temporalExtents = c("1985_2018", "1985_1990", "1991_1995", "1996_1999", "2000_2004", "2005_2012", "2013_2018") 
# vector with all spatial scales (all biomes + brazil)
spatialExtents = c("Brazil", "Amazonia", "Caatinga", "Cerrado", "MataAtlantica", "Pampa", "Pantanal") 
# variables of interest
responseVars = c("forest_original", "forest_change")
controlVars = c("private","public")
treatmentVars = c("private","public", "protected", "sustainable_use", "indigenous", "communal", "quilombola")

# define covariates used in matching
matchingVars = c("elevation", "slope", "travel_time", "population_dens")

# define covariates used in statistical models on matched data
nonspatial_modelCovars = c(matchingVars, "state", "area")
spatial_modelCovars = c(nonspatial_modelCovars, "rac") # residuals autocovariate is calculated from the non spatial models

# define manual cutpoints (for making bins) for the coarsened matching
travel_cut = c(0,240,360,720,1440) # by hours (0,2,6,12,24)
area_cut = c(0,2,5,15,50,100,500,1000,5000,10000,50000,100000,500000,1000000) # 14 classes of property sizes 
todrop = c("gid", "tenure", "state", "biome")

# function to define re-categorization of Brazil's tenure forms to ours. 
reclassify_tenuredata <- function(data){
data$tenure <- ifelse (data$subclass == "CAR poor", "private",
               ifelse (data$subclass == "Transporte", "other",
               ifelse (data$subclass == "Terra Legal titulado", "private",
               ifelse (data$subclass ==  "CAR premium", "private",
               ifelse (data$subclass == "Não destinado", "public", 
               ifelse (data$subclass == "Urbano", "other",
               ifelse (data$subclass == "Assentamento rural", "public",
               ifelse (data$subclass == "SIGEF", "private",
               ifelse (data$subclass == "Território comunitário", "communal",
               ifelse (data$subclass == "Água", "other",
               ifelse (data$subclass == "Terra Legal não titulado", "public",
               ifelse (data$subclass == "Terra Indígena homologada", "indigenous",
               ifelse (data$subclass == "UC Proteção Integral", "protected",
               ifelse (data$subclass == "UC Uso Sustentável", "sustainable_use",
               ifelse(data$subclass == "Terra Quilombola","quilombola",   
               ifelse (data$subclass == "Terra Indígena não homologada", "indigenous", "other"))))))))))))))))
return(data)
}



