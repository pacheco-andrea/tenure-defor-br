# ****************************** #
# regression analysis 
# ****************************** #
# this script runs GLMS for all our spatiotemporal scales to check for any bias in the original amt. of forest per parcel
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
    
    # check for biases in forest cover of parcels across comparison groups
    myglm2 <- miceadds::glm.cluster(data = data, formula = as.formula(paste0("cbind(for_orig, (count-for_ch)) ~ ",
                                                                            names(data)[grep("_vs_", names(data))],
                                                                            " + elevation + slope + travel_time + pop + area + state")),
                                   cluster="mun", family = binomial(link = "logit"))
  }
  if(length(unique(data$state)) <= 1){ # if there is less than one state, run this adjusted model that does not control for state as a factor
    
    # check for biases in forest cover of parcels across comparison groups
    myglm2 <- miceadds::glm.cluster(data = data, formula = as.formula(paste0("cbind(for_orig, (count-for_ch)) ~ ",
                                                                             names(data)[grep("_vs_", names(data))],
                                                                             " + elevation + slope + travel_time + pop + area")),
                                    cluster="mun", family = binomial(link = "logit"))
  }
  
  setwd(paste0(wd_main, "/outputs/GLMs_forestAreaBias/"))
  saveRDS(myglm2, paste0(n, "_logits", ".rds"))
  
  # calculate average marginal effects
  ame2 <- summary(margins(model=myglm2$glm_res, vcov= myglm2$vcov, data=data, type = "response"))  # to keep on response variable scale (%)
  ame2$n <- nrow(data)
  setwd(paste0(wd_main, "/outputs/AMEs_forestAreaBias/"))
  write.csv(ame2, paste0(n, "_ame", ".csv"))
  
}




