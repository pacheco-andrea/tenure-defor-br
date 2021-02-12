# ****************************** #
# regression analysis 
# ****************************** #
# this script runs GLMS for all our spatiotemporal scales
# i "backstransform" logits into average marginal effects using the margins packages

library("stats")
library("margins")
library("miceadds")

# working dirs
wd_main <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/P1/"

# job:
setwd(paste0(wd_main, "inputs/01_matched/forStatsAnalysis"))
input <- list.files()
i=as.integer(Sys.getenv('SGE_TASK_ID')) # 553

data <- read.csv(input[i])
n <- gsub(".csv", "", input[i])

# rename population variable to apply on all datasets
data$pop <- data[,grep("[0-9]_",colnames(data))]
# mark as NA values for which it was not possible to extract forest 
data$for_ch[which(data$for_ch < 0)] <- NA

# glms
 
if(nrow(data) > 40){ # if the data frame has more than 40 observations,
  
  if(length(unique(data$state)) > 1){ # if there is more than one state in this dataset, run the normal model:
    
    myglm <- miceadds::glm.cluster(data = data, formula = as.formula(paste0("cbind(for_ch, (for_orig-for_ch)) ~ ",
                                                                            names(data)[grep("_vs_", names(data))],
                                                                            " + elevation + slope + travel_time + pop + area + state")),
                                   cluster="mun", family = binomial(link = "logit"))
  }
  if(length(unique(data$state)) <= 1){ # if there is less than one state, run this adjusted model that does not control for state as a factor
    
    myglm <- miceadds::glm.cluster(data = data, formula = as.formula(paste0("cbind(for_ch, (for_orig-for_ch)) ~ ",
                                                                            names(data)[grep("_vs_", names(data))],
                                                                            " + elevation + slope + travel_time + pop + area")),
                                   cluster="mun", family = binomial(link = "logit"))
  }
  setwd(paste0(wd_main, "/outputs/GLMs_noTL_clusterMun/"))
  saveRDS(myglm, paste0(n, "_logits", ".rds"))
  
  # calculate average marginal effects
  ame <- summary(margins(model=myglm$glm_res, vcov= myglm$vcov, data=data, type = "response"))  # to keep on response variable scale (%)
  ame$n <- nrow(data)
  setwd(paste0(wd_main, "/outputs/AMEs_noTL_clusterMun/"))
  write.csv(ame, paste0(n, "_ame", ".csv"))
}

if(!exists("myglm")){
  setwd(paste0(wd_main, "/outputs/noTL_clusterMun_errors/"))
  if(nrow(data) <= 40){
    write.csv(data, paste0(n, "_Nerror.csv"))
  }else{
    write.csv(data, paste0(n, "_OtherError.csv"))}
}




