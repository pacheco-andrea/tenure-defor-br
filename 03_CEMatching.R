# ************************************************************ #
# Run matching with CEM as job
# job is 588 datasets
# ************************************************************ #

library("cem")

#working dirs
wd_main <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/P1"
wd_in <- paste0(wd_main, "/inputs/00data/for_matching/forMatchAnalysisCEM")
wd_out <- paste0(wd_main, "/outputs/MatchedDatasets")


travel_cut = c(0,240,360,720,1440) # by hours (0,2,6,12,24)
area_cut = c(0,2,5,15,50,100,500,1000,5000,10000,50000,100000,500000,1000000) # 14 classes of property sizes 
todrop = c("gid", "tenure", "name", "state", "mun", "biome") #edit

# job:
setwd(wd_in)
input <- list.files()
i=as.integer(Sys.getenv('SGE_TASK_ID'))
# read data
data <- read.csv(input[i])
n <- gsub(".csv", "", input[i])

#match
drop_this <- c(todrop, paste0(colnames(data)[grep("_vs_",colnames(data))]))

tmp_match <- try(cem(treatment = colnames(data)[grep("_vs_",colnames(data))], 
                     data = as.data.frame(data),
                     drop = drop_this,
                     cutpoints = list(travel_time = travel_cut, area = area_cut),
                     keep.all = T,
                     k2k = T,
                     method = "euclidean"), silent = T) 
setwd(wd_out)
if(class(tmp_match) == "try-error"){saveRDS(tmp_match, paste0(n, "_try-error", ".rds"))}
if(class(tmp_match) == "cem.match"){saveRDS(tmp_match, paste0(n, ".rds"))}

