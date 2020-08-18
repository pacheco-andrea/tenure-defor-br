library("cem")

# working dirs
wd_main <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/matching/"
wd_in <- paste0(wd_main, "allExt/input")
wd_out <- paste0(wd_main, "allExt/output")

# parameters
travel_cut = c(0,240,360,720,1440) # by hours (0,2,6,12,24)
area_cut = c(0,2,5,15,50,100,500,1000,5000,10000,50000,100000,500000,1000000) # 14 classes of property sizes 
todrop = c("gid", "tenure", "state", "biome")

# job:
setwd(wd_in)
input <- list.files()
i=as.integer(Sys.getenv('SGE_TASK_ID'))

setwd(wd_in)
data <- read.csv(input[i])
n <- gsub(".csv", "", input[i])

#match
drop_this <- c(todrop, paste0(colnames(data[10])))
tmp_match <- try(cem(treatment = colnames(data[10]), 
                     data = as.data.frame(data),
                     drop = drop_this,
                     cutpoints = list(travel_time = travel_cut, area = area_cut),
                     keep.all = T,
                     k2k = T,
                     method = "euclidean"), silent = T) 
setwd(wd_out)
if(class(tmp_match) != "try-error"){saveRDS(tmp_match, paste0(n, ".rds"))} else {saveRDS(tmp_match, paste0(n, "_try-error", ".rds"))}