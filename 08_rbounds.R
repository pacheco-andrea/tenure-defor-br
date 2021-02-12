# ************************************************************ #
# Compute Rosenbaum bounds
# ************************************************************ #
# sensitivity parameter "gamma" that measures the degree of departure fro random assignment of treatment. 
# a pair of observations may differ in odds of receiving the treatment by a factor of "gamma"
# In a randomized experiment, randomization of the treatment ensures gamma = 1. 
# In an observational study, if gamma = 2, and two subjects are identical on matched covariates,
# then one might be twice as likely as the other to receive the treatment because they differ in terms of an unobserved covariate
# 2 options: bounds for pvalues, bounds for treatment effects 


library("stats")
library("margins")
library("miceadds")
library("rbounds")

# working dirs
wd_main <- "/gpfs1/data/idiv_meyer/01_projects/Andrea/P1/"

# job:
setwd(paste0(wd_main, "outputs/GLMs_noTL_clusterMun"))
input <- list.files()
i=as.integer(Sys.getenv('SGE_TASK_ID')) # 553

# get glm 
myglm <- readRDS(input[i])
n <- gsub("_logits.rds", "", input[i])

# sensitivity test for our AME results:

# calculate AMEs again
mAME <- margins(model=myglm$glm_res, vcov= myglm$vcov, data=myglm$glm_res$data, type = "response")

start <- grep("^p", names(mAME))
dydx <- grep("^dydx", names(mAME))
treat <- grep("_vs_", names(mAME)[start])
fitted_tenure <- mAME[[names(mAME)[dydx][treat]]]

names(mAME[start][treat])

# treatment group coefficients 
fittedTRT <- fitted_tenure[which(mAME[[names(mAME[start][treat])]] == 1)]
# control group coefficients
fittedCTRL <- fitted_tenure[which(mAME[[names(mAME[start][treat])]] == 0)]
# calculate hlsens
AME_hl <- hlsens(x=fittedTRT, y=fittedCTRL, Gamma=5, GammaInc=1)
AME_hl$bounds

AME_p <- psens(x=fittedTRT, y=fittedCTRL, Gamma=5, GammaInc=1)
AME_p$bounds

mybounds <- data.frame("Gamma"=AME_hl$bounds$Gamma, "hl_lower"= AME_hl$bounds[,2], "hl_upper"= AME_hl$bounds[,3],
           "pval_lower"= AME_p$bounds[,2], "pval_upper"= AME_p$bounds[,3])

# write out
if(exists("AME_hl")){
  setwd(paste0(wd_main, "outputs/rbounds"))
  write.csv(mybounds, paste0(n, ".csv"))
}



