# ****************************** #
# Non-spatial Statisical Analysis 
# ****************************** #
# this script conducts non spatial glm's 
# these are not the final analysis, but we need to conduct them in order to calculate an RAC
# it is carried out on the HPC
library("stats")

# working dirs
wd_main <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/nonSp_glms/"
wd_in <- paste0(wd_main, "input")
wd_out <- paste0(wd_main, "output")

# job ----
setwd(wd_in)
input <- list.files()
i=as.integer(Sys.getenv('SGE_TASK_ID'))

setwd(wd_in)
data <- read.csv(input[i])
n <- gsub(".csv", "", input[i])

# glms ----
 
if(nrow(data) > 1){ # if the data frame has more than one observation,
  
  if(length(unique(data$state)) > 1){ # if there is more than one state in this dataset, run the normal model:
    
    myglm <- glm(as.formula(paste0("cbind(for_ch, (for_orig-for_ch)) ~ ",names(data)[grep("_vs_", names(data))]," + elevation + slope + travel_time + pop + area + factor(state)")),
                   data = data, family = binomial(link = "logit"))
  }
  if(length(unique(data$state)) < 1){ # if there is less than one state, run this adjusted model that does not control for state as a factor
    
    myglm <- glm(as.formula(paste0("cbind(for_ch, (for_orig-for_ch)) ~ ",names(data)[grep("_vs_", names(data))]," + elevation + slope + travel_time + pop + area")),
                   data = data, family = binomial(link = "logit"))
  }

}
setwd(wd_out)
saveRDS(myglm, paste0(n, ".rds"))
