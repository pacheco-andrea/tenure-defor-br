library("cem")

wd_main <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/P1/"
wd_mtables <- paste0(wd_main, "inputs/00data/for_matching/forMatchAnalysisCEM")

# job:
setwd(paste0(wd_main, "outputs/MatchedDatasets"))
input <- list.files()
i=as.integer(Sys.getenv('SGE_TASK_ID')) # 588
m <- readRDS(input[i])
name1 <- gsub(".rds", "", input[i])

# if condition, if match ran, then:
if(class(m) == "cem.match"){
  # add "matched" column to original dataframes
  setwd(paste0(wd_main, "inputs/00data/for_matching/forMatchAnalysisCEM"))
  matcheslist <- list.files()
  # ensure get match that corresponds
  match_needed <- match(name1, gsub(".csv", "", matcheslist))
  df <- read.csv(matcheslist[match_needed])
  name2 <- gsub(".csv","", matcheslist[match_needed])
  
  df$matched <- m$matched
  
  # create two datasets
  data <- df
  mdata <- data[which(data$matched == "TRUE"),]
  # variables that we do NOT match on 
  todrop = c("gid", "tenure", "name", "state", "mun", "biome") #edit
  drop_this <- c(todrop, paste0(colnames(data)[grep("_vs_",colnames(data))]), "matched")
  # same cutpoints as before
  travel_cut = c(0,240,360,720,1440) # by hours (0,2,6,12,24)
  area_cut = c(0,2,5,15,50,100,500,1000,5000,10000,50000,100000,500000,1000000) # 14 classes of property sizes 
  
  
  # imbalance of unmatched
  i_data <- imbalance(group = data[,names(data)[grep("_vs_", names(data))]], data = data, breaks = list(travel_time = travel_cut, area = area_cut), drop = drop_this)
  # imbalance of matched
  i_mdata<- imbalance(group = mdata[,names(mdata)[grep("_vs_", names(mdata))]], data = mdata, breaks = list(travel_time = travel_cut, area = area_cut), drop = drop_this)
  # table to report
  imbalancedf <- data.frame("before" = (i_data$L1$L1), "after" = (i_mdata$L1$L1), "L1" = (i_data$L1$L1)-(i_mdata$L1$L1))
  # write imbalance
  setwd(paste0(wd_main, "outputs/imbalance"))
  write.csv(imbalancedf, paste0(name1, ".csv"))
  
  # write only matched dataset
  setwd(paste0(wd_main, "outputs/MatchedDatasets_onlymatches"))
  write.csv(mdata, paste0(name1, ".csv"))
}

