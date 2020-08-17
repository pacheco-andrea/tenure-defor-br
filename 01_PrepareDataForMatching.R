# ************************************************************ #
# WORKFLOW FOR PREPPING BRAZIL'S TENURE DATA FOR MATCHING ####
# ************************************************************ #

# This script reads in shapefiles that have the unique ID (gid) for all parcels in Brazil, for one biome at a time in order to manage computational sizes. (This is part A)
# It reads in extractions of covariates that Ruben produced on the cluster. (This is part B)
# It puts these two pieces of info together, and reads in the other tenure info from Imaflora as sqlite databases. (This is part C)
# It puts all this info together, and reclassifies all original tenure forms into our actually 7 categories.
# the output is shapefiles for each biome with columns ready for matching and new tenure categories

setwd("~/data/MAS-group-share/04_personal/Andrea/P1_analysis/code/")
source("00_Setting_Parameters.R")


# READ IN DATA ----

wd_env_extractions <- ("~/data/MAS-group-share/04_personal/Andrea/P1_analysis/inputs/00_data/env/")
wd_shapes <- ("~/data/MAS-group-share/04_personal/Andrea/P1_analysis/inputs/00_data/shp/")
wd_tenurelite <- ("~/data/MAS-group-share/04_personal/Andrea/P1_analysis/inputs/00_data/tenure_lites/")
wd_output <- ("~/data/MAS-group-share/04_personal/Andrea/P1_analysis/inputs/00_data/for_matching/forMatchAnalysis/")


# shapefiles that only have gid were pre-processed. read in only dbfs (tables with only id's):
setwd(wd_shapes)
gids <- list.files()
gids <- gids[grep(".dbf",gids)] 
gids <- lapply(gids, read.dbf)
gids_brasil <- bind_rows(gids)

# environmental extractions (pre-processed on the HPC)
setwd(paste0(wd_env_extractions, "other_fixed_covars/"))
env_data <- list.files()
env_data <- lapply(env_data, read_csv)
env_data_brasil <- bind_rows(env_data)
env_data_brasil <- cbind(gids_brasil, env_data_brasil[,2:4])

# population data ----
setwd(paste0(wd_env_extractions, "population_1990-2015/"))
pop_data <- list.files()
pop_data <- lapply(pop_data, read_csv)
pop_data <- bind_rows(pop_data)
summary(pop_data)
pop_data <- pop_data[which(pop_data$p1975 >= 0),]
summary(pop_data)

# interpolate/extrapolate population averages for time periods we need 
tempExtent <- c(1985,2018) # max and min extent
pop_IntExtrapolate <- function(pop_data, tempExtent){
  
  years <- as.numeric(gsub("p","", names(pop_data)[grep("^p[0-9]", names(pop_data) )]))
  data <- data.frame("gid" = pop_data$gid)
  
  if(tempExtent[1] < years[1]){ # if we need to extrapolate before the first data point, then create columns
    for(i in 1:(years[1] - tempExtent[1]))
    {
      data[,paste0("e", tempExtent[1]+i-1)] <- NA
    }
  }
  for(i in 1:length(years)) # including the data we have, and create interpolation columns
  {
    data[,paste0("p", years[i])] <- pop_data[,paste0("p", years[i])]
    if(i!=length(years)){
      for(j in 1:(years[i+1] - years[i]-1))
      {
        data[,paste0("i", years[i]+j)] <- NA
      }
    }
  }
  if(tempExtent[2] > years[length(years)]){ # if we need to extrapolate after the last data point, then create columns needed 
    for(i in 1:(tempExtent[2] - years[length(years)]))
    {
      data[,paste0("e", years[length(years)]+i)] <- NA
    }
  }
  
  intervals <- grep("^p",names(data))
  for(i in 1:(length(years)-1)) # for all intervals available
  {
    # first year - last year for absolute change:
    inc <- (data[,intervals[i+1]]-data[,intervals[i]])/(intervals[i+1] - intervals[i]) # calculate the "x", i.e. the increase 
    # (last year there's data - first year there's data ) / (amount of years there's no data: second interval - first interval) 
    if(i!=length(intervals)){
      
      for(j in 1:(intervals[i+1] - intervals[i]-1)) # years with no data
      {
        data[,paste0("i", years[i]+j)] <- data[,paste0("p", years[i])] + (j*inc)
        # column in the data which is the first year there's data + 1 at a time <- populate column with value of the first interval year + the increase amount (* the amount of times the loop has run)
      }
      if(i==1) {
        if(tempExtent[1] < years[1]){
          for(k in 1:(years[1] - tempExtent[1]))
          {
            data[,paste0("e", tempExtent[1]+k-1)] <- data[,paste0("p", years[1])] - (seq((years[1] - tempExtent[1]), 1, -1)[k] * inc)     # (((years[1] - tempExtent[1])- 1 + k )*inc)
            # data of base year - the increase that we've calculated * 5-4-3-2-1
          }
          
        }
      }
      if(i==length(years)-1){
        if(tempExtent[2] > years[length(years)]){
          for(k in 1:(tempExtent[2] - years[length(years)]))
          {
            data[,paste0("e", years[length(years)]+k)] <- data[,paste0("p", years[length(years)])] + (k*inc)
          }
        }
      }
      
    }
  }
  return(data) # returns dataframe where p is the real value, i is interpolated, and e is extrapolated
}
pop_data_new <- pop_IntExtrapolate(pop_data, tempExtent)
summary(pop_data_new)

for(i in 2:length(pop_data_new)) # fix negative values: we will simply leave at 0 (no negative population densities)
{
  pop_data_new[i][which(pop_data_new[i] <0),] <- 0
}
summary(pop_data_new)


myPopData <- function(output_int.ext, temporalExtents){
  
  years <- as.numeric(gsub("\\D", "", colnames(output_int.ext)[grep("[0-9]", colnames(output_int.ext))] )) # get only the years (number) we've interpolated or extrapolated
  
  data <- data.frame("gid" = output_int.ext$gid)
  
  for(i in 1:length(temporalExtents))
  {
    data[,paste0(temporalExtents[i])] <- NA # create columns needed
  }
  head(data)
  
  data$`1985_2018` <- rowMeans(output_int.ext[, 12:length(output_int.ext)])
  data$`1985_1990` <- rowMeans(output_int.ext[, 12:17])
  data$`1991_1995` <- rowMeans(output_int.ext[, 18:22])
  data$`1996_1999` <- rowMeans(output_int.ext[, 23:26])
  data$`2000_2004` <- rowMeans(output_int.ext[, 27:31])
  data$`2005_2012` <- rowMeans(output_int.ext[, 32:39])
  data$`2013_2018` <- rowMeans(output_int.ext[, 40:length(output_int.ext)])
  
  return(data)
}
pop_data_brasil <- myPopData(pop_data_new, temporalExtents)

# bind population together w environmental covariate data
env_coVars_brasil <- merge(env_data_brasil, pop_data_brasil, by = "gid")
summary(env_coVars_brasil)
nrow(env_coVars_brasil)
env_coVars_brasil <- drop_na(env_coVars_brasil) # we lose 398 observations due to NA's

neg1 <- env_coVars_brasil[which(env_coVars_brasil$p1985_1990 < 0),] 
nrow(neg1)
summary(env_coVars_brasil)

# recategorize tenure data ----
# originally in biome sqlites
setwd(wd_tenurelite)
con <- dbConnect(drv = RSQLite::SQLite(), dbname = "Amazonia_lite.sqlite") # connect to database for sqlite reading
con_amazonia <- dbReadTable(con, "amazonia_lite")
dbDisconnect(con)

con <- dbConnect(drv = RSQLite::SQLite(), dbname = "Caatinga_lite.sqlite") # connect to database for sqlite reading
con_caatinga <- dbReadTable(con, "caatinga_lite")
dbDisconnect(con)

con <- dbConnect(drv = RSQLite::SQLite(), dbname = "Cerrado_lite.sqlite") # connect to database for sqlite reading
con_cerrado <- dbReadTable(con, "cerrado_lite")
dbDisconnect(con)

con <- dbConnect(drv = RSQLite::SQLite(), dbname = "MataAtlantica_lite.sqlite") # connect to database for sqlite reading
con_mata <- dbReadTable(con, "mataatlantica_lite")
dbDisconnect(con)

con <- dbConnect(drv = RSQLite::SQLite(), dbname = "Pampa_lite.sqlite") # connect to database for sqlite reading
con_pampa <- dbReadTable(con, "pampa_lite")
dbDisconnect(con)

con <- dbConnect(drv = RSQLite::SQLite(), dbname = "Pantanal_lite.sqlite") # connect to database for sqlite reading
con_pantanal <- dbReadTable(con, "pantanal_lite")
dbDisconnect(con)

tenure_brazil <- bind_rows(con_amazonia, con_caatinga, con_cerrado, con_mata, con_pampa, con_pantanal)
nrow(tenure_brazil)
unique(tenure_brazil$subclass)

# reclassify 
tenure_brazil <- reclassify_tenuredata(tenure_brazil)
tenure_brazil_summary <- data.frame(cbind("gid" = tenure_brazil$gid, "tenure" = tenure_brazil$tenure, "area" = tenure_brazil$area_fin, "state" = tenure_brazil$nm_uf, "biome" = tenure_brazil$bioma))
head(tenure_brazil_summary)
tenure_brazil_summary$area <- as.numeric(as.character(tenure_brazil_summary$area))

# finalize needed df's for matching ----
# put all data together, and then write as many separate dataframes for matching at every specific spatiotemporal scale: 
allAnalysisData <- merge(env_coVars_brasil, tenure_brazil_summary, by = "gid")
# careful manual separation of needed columns:
allAnalysisData_brazil_1985_2018 <- subset(allAnalysisData, select = -c(6:11))
allAnalysisData_brazil_1985_1990 <- subset(allAnalysisData, select = -c(5,7:11))
allAnalysisData_brazil_1991_1995 <- subset(allAnalysisData, select = -c(5:6,8:11))
allAnalysisData_brazil_1996_1999 <- subset(allAnalysisData, select = -c(5:7,9:11))
allAnalysisData_brazil_2000_2004 <- subset(allAnalysisData, select = -c(5:8,10:11))
allAnalysisData_brazil_2005_2012 <- subset(allAnalysisData, select = -c(5:9,11))
allAnalysisData_brazil_2013_2018 <- subset(allAnalysisData, select = -c(5:10))
# create lists of extents
allAnalysisData_brazil_temporalextents <- list(allAnalysisData_brazil_1985_2018, 
                                               allAnalysisData_brazil_1985_1990, 
                                               allAnalysisData_brazil_1991_1995, 
                                               allAnalysisData_brazil_1996_1999, 
                                               allAnalysisData_brazil_2000_2004,
                                               allAnalysisData_brazil_2005_2012,
                                               allAnalysisData_brazil_2013_2018)
names(allAnalysisData_brazil_temporalextents) <- temporalExtents
# write temporal
setwd(wd_output)
saveRDS(allAnalysisData_brazil_temporalextents, "allAnalysisData_brazil_temporalextents.rds")
# spatial extents only need to be split by biome
allAnalysisData_spatialextents_1985_2018 <- split(allAnalysisData_brazil_1985_2018, allAnalysisData_brazil_1985_2018$biome)
# write spatial
setwd(wd_output)
write_rds(allAnalysisData_spatialextents_1985_2018, "allAnalysisData_spatialextents_1985_2018.rds")
# write spatio-temporal
allAnalysisData_spatialextents_temporalextents <- list()
for(i in 1:length(allAnalysisData_brazil_temporalextents))
{
  allAnalysisData_spatialextents_temporalextents[[i]] <- split(allAnalysisData_brazil_temporalextents[[i]], allAnalysisData_brazil_temporalextents[[i]]$biome)
}
names(allAnalysisData_spatialextents_temporalextents) <- temporalExtents
setwd(wd_output)
write_rds(allAnalysisData_spatialextents_temporalextents, "allAnalysisData_spatialextents_temporalextents.rds")

