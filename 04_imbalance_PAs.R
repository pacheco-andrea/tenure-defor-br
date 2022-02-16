library("cem")

wd_main <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/P1/"

# job:
setwd(paste0(wd_main, "outputs/MatchedDatasets_PA"))
input <- list.files()
i=as.integer(Sys.getenv('SGE_TASK_ID')) # 196
m <- readRDS(input[i])
if(class(m) == "cem.match"){
  
  name1 <- gsub(".rds", "", input[i])
  
  # add "matched" column to original dataframes
  setwd(paste0(wd_main, "outputs/MatchedPADatasets_yearCreatedSubset"))
  matcheslist <- list.files()
  # ensure get match that corresponds
  match_needed <- match(name1, gsub("_YrSubset.csv", "", matcheslist))
  df <- read.csv(matcheslist[match_needed])
  name2 <- gsub("_YrSubset.csv","", matcheslist[match_needed])
  
  df$matched <- m$matched
  
  # create two datasets
  data <- df
  mdata <- data[which(data$matched == "TRUE"),]
  # variables that we do NOT match on 
  todrop = c("gid", "tenure", "name", "state", "mun", "biome") #edit
  drop_this <- c(todrop, paste0(colnames(data)[grep("_vs_",colnames(data))]), "matched", "yearCreated")
  
  # imbalance of unmatched
  i_data <- imbalance(group = data[,names(data)[grep("_vs_", names(data))]], data = data, drop = drop_this)
  # imbalance of matched
  i_mdata<- imbalance(group = mdata[,names(mdata)[grep("_vs_", names(mdata))]], data = mdata, drop = drop_this)
  # table to report
  imbalancedf <- data.frame("before" = (i_data$L1$L1), "after" = (i_mdata$L1$L1), "L1" = (i_data$L1$L1)-(i_mdata$L1$L1))
  # write imbalance table
  setwd(paste0(wd_main, "outputs/imbalancePA"))
  write.csv(imbalancedf, paste0(name1, ".csv"))
  
  # write only matched dataset
  setwd(paste0(wd_main, "outputs/MatchedDatasets_onlymatches_PA"))
  write.csv(mdata, paste0(name1, ".csv"))

}
