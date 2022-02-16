# ****************************** #
# Synthesis tables (for supplementary materials)
# ****************************** #
# this script creates synthesis tables reporting results
# we report two things "effect direction" and "effect magnitude"
# effect direction counts how many models point in the direction of i) increasing deforestation and ii) decreasing deforestation. 
# This answers the question of how reliably do we find effects in either direction?
# effect magnitude ranks relative effect sizes (from negative to positive), for each comparison set at each scale considered. 
# then, it calculates across scales the % of the time that a tenure regime is best for forests and worst for forests
# we repeat the same synthesis for several versions 1) comparing consistent scales 2) using our filtered results 3)

library(readr)
library(dplyr)
library(data.table)

# working dirs
#wdmain <- "~/data/MAS-group-share/04_personal/Andrea/P1_analysis/outputs/"
wdmain <- "I:/MAS/04_personal/Andrea/P1_analysis/outputs/"


# working dirs
wdmain <- "~/data/MAS-group-share/04_personal/Andrea/P1_analysis/"

# get data
setwd(paste0(wdmain, "outputs/model_summaries/"))
mypublic_table <- read.csv("TS3_allPublicModels_TATE.csv")
myprivate_table <- read.csv("TS4_allPrivateModels_TATE.csv")
mypublic_table_altPA <- read.csv("TS3_allPublicModels_TATE_PA.csv")
myprivate_table_altPA <- read.csv("TS4_allPrivateModels_TATE_PA.csv")

# synthesis 1: table that summarizes EFFECT DIRECTION ----

# create a function to count models by looping through each comparison
# i=1
# control_table = myprivate_table[which(myprivate_table$Treatment != "Communal"),]
# treatment = unique(control_table$Treatment)[i]
# control="private"
# effect = "ame"
# effect = "tate"

getLCD <- function(control_table, treatment, control){
  
  # select data that is one specific comparison
  mydata <- control_table[which(control_table$Treatment == treatment),] 
  #mydata <- na.omit(mydata)
  # select data that have more than 40 observations
  mydata <- mydata[which(mydata$N >= 40),]
  
  # exclude 2 biomes (pampa and pantanal)
  mydata <- mydata[which(mydata$spatialScale != "Pantanal"),]
  mydata <- mydata[which(mydata$spatialScale != "Pampa"),]
  
  # exclude model that's for all brazil for entire time period 
  mydata <- unite(mydata, col = sptemp_scale, c("spatialScale","temporalScale"), sep = "_", remove = F)
  mydata <- mydata[which(mydata$sptemp_scale != "Brazil_1985-2018"),]
  possibleScales <- unique(mydata$sptemp_scale)
  return(possibleScales)
}
# scales = 
get_modelcounts <- function(control_table, treatment, scales, effect){
  
  # for each treatment at a time
  mydata <- control_table[which(control_table$Treatment == treatment),] 
  # create sptemp scale
  mydata <- unite(mydata, col = sptemp_scale, c("spatialScale","temporalScale"), sep = "_", remove = F)
  #exclude non-consistent scales
  mydata <- mydata[which(mydata$sptemp_scale != "Brazil_1985-2018"),]
  
  if(!is.null(scales)){
    mydata <- inner_join(mydata, data.frame("sptemp_scale"=scales), by="sptemp_scale")
  }
  
  # deal with statistically nonsignificant values
  # replace Effects with 0, because they are statistically indistinguishable from 0
  
  if(effect == "ame"){
    eff_pos <- grep("Effect", colnames(mydata))
    p_pos <- grep("^p_value$", colnames(mydata))
  }
  if(effect == "tate"){
    eff_pos <- grep("TATE$", colnames(mydata))
    p_pos <- grep("TATE_p_value$", colnames(mydata))
  }
  #mydata$OrigEffect <- mydata[,eff_pos]
  mydata[,eff_pos][which(mydata[,p_pos] >= 0.05)] <- 0
  # only include in dataset models which were calculated
  mydata <- mydata[which(!is.na(mydata[,p_pos])),]
  
  # increase deforestation
  inc <- nrow(mydata[which(mydata[,eff_pos] > 0),]) 
  p.inc <- inc/nrow(mydata)
  # decrease deforestation
  dec <- nrow(mydata[which(mydata[,eff_pos] < 0),]) 
  p.dec <- dec/nrow(mydata)
  # how many nonsignificant?
  nonsig <- nrow(mydata[which(mydata[,p_pos] >= 0.05),]) 
  p.nonsig <- nonsig/nrow(mydata)
  # how many models did we have in total (>40, not all br-85-18)
  n_mod <- nrow(mydata) 
  
  # same measures weighted by balance
  # weighted sum of effects which increase deforestation
  b_inc <- sum(1-(mydata$ImbAfter[which(mydata[,eff_pos] > 0)])) # subtract 1-imbalance, so that it become a simple balance metric, and value reflects "validity" of effect
  # this is only a sum of the weights because this metric is a COUNT. ((1*w)+(1*w)+...)
  # weighted percentage
  p.b_inc <- b_inc/(sum(1-(mydata$ImbAfter))) # should be divided by sum of all weights!!!!!!!!!!
  # weighted sum of effects which decrease deforestation
  b_dec <- sum(1-(mydata$ImbAfter[which(mydata[,eff_pos] < 0)]))
  p.b_dec <- b_dec/(sum(1-(mydata$ImbAfter)))
  
  # weighted nonsignificant
  b_nonsig <- sum(1-mydata$ImbAfter[which(mydata[,p_pos] >= 0.05)]) # sum of "counts" of nonsig models weighted by balance
  p.b_nonsig <- b_nonsig/(sum(1-(mydata$ImbAfter)))
  
  sum(p.b_inc,p.b_dec,p.b_nonsig) # should be ==1
  # total balance
  Tbal <- (sum(1-(mydata$ImbAfter)))
  
  mytable <- cbind(inc, b_inc, p.inc, p.b_inc, 
                   dec, b_dec, p.dec, p.b_dec,
                   nonsig,b_nonsig, p.nonsig, p.b_nonsig, n_mod, Tbal)
  rownames(mytable) <- treatment
  mytable
  return(mytable)
}

# loop through treatments to apply both functions
# test:
# control_table = mypublic_table[which(mypublic_table$Treatment != "Communal"),]
getMyCounts <- function(control_table, consistentScales, effect){
  
  if(consistentScales==T){
    
    # before anything else, I need to exclude the row of sustainable use vs. private - Caatinga 1985-2018, because we weren't able to estimate it at all at one scale 
    # rendering this a non-consistent scale. 
    control_table$rm <- NA
    if(effect == "ame"){
      p_pos <- grep("^p_value$", colnames(control_table))
    }
    if(effect == "tate"){
      p_pos <- grep("TATE_p_value", colnames(control_table))
    }
    control_table$rm <- ifelse(is.na(control_table[,p_pos]),T, F)# specify the if condition to be sensitive to p-values being NA - i.e. a model that wasn't calculated
    control_table <- control_table[which(control_table$rm != T),]
    
    # first get scales that are needed
    commonDenominators <- list()
    for(i in 1:length(unique(control_table$Treatment))) # start at 2 because we want to ignore communal
    {
      commonDenominators[[i]] <- getLCD(control_table, treatment = unique(control_table$Treatment)[i])
    }
    # Reduce, and intersect calculates where all these scales intercept!
    myScales <- Reduce(intersect, commonDenominators)
    print(length(myScales))
  }
  if(consistentScales==F){
    myScales <- NULL
  }
  
  # get counts using scales
  count <- list()
  for(i in 1:length(unique(control_table$Treatment)))
  {
    count[[i]] <- get_modelcounts(control_table, treatment = unique(control_table$Treatment)[i], scales = myScales, effect = effect)
  }
  efdirections <- do.call("rbind", count)
  return(efdirections)
}

# Table S5 (consistent scales)
public_count_AME <- getMyCounts(mypublic_table[which(mypublic_table$Treatment != "Communal"),], consistentScales=T, effect = "ame")
public_count_AME[c(2,3,5,1,4),]
private_count_AME <- getMyCounts(myprivate_table[which(myprivate_table$Treatment != "Communal"),], consistentScales=T, effect = "ame")
private_count_AME[c(5,2,4,1,3),]

setwd(paste0(wdmain, "outputs/model_summaries/"))
write.csv(public_count_AME[c(2,3,5,1,4),], "effectdirection_public_TATE.csv")
write.csv(private_count_AME[c(5,2,4,1,3),], "effectdirection_private_TATE.csv")

# Table S6 (consistent scales to the level of filtered PA/sustuse)
altPA.public_count <- getMyCounts(mypublic_table_altPA[which(mypublic_table_altPA$Treatment != "Communal"),], consistentScales=T, effect = "ame")
altPA.public_count[c(2,3,5,1,4),]
altPA.private_count <- getMyCounts(control_table = myprivate_table_altPA[which(myprivate_table_altPA$Treatment != "Communal"),], consistentScales=T, effect = "ame")
altPA.private_count[c(5,2,4,1,3),]

setwd(paste0(wdmain, "outputs/model_summaries/"))
write.csv(altPA.public_count[c(2,3,5,1,4),], "effectdirection_public_altPA.csv")
write.csv(altPA.private_count[c(5,2,4,1,3),], "effectdirection_private_altPA.csv")


# Table S7 (all scales)
public_count_S7 <- getMyCounts(mypublic_table, consistentScales=F, effect = "ame")
public_count_S7[c(3,4,6,2,5,1),]
private_count_S7 <- getMyCounts(myprivate_table, consistentScales=F, effect = "ame")
private_count_S7[c(6,3,5,2,4,1),]

altPA.public_count_S7 <- getMyCounts(mypublic_table_altPA, consistentScales=F, effect = "ame")
altPA.public_count_S7[c(3,5,6,2,4,1),]
altPA.private_count_S7 <- getMyCounts(myprivate_table_altPA, consistentScales=F, effect = "ame")
altPA.private_count_S7[c(3,5,6,2,4,1),]

setwd(paste0(wdmain, "outputs/model_summaries/"))
write.csv(public_count_S7[c(3,4,6,2,5,1),], "effectdirection_public_S7.csv")
write.csv(private_count_S7[c(6,3,5,2,4,1),], "effectdirection_private_S7.csv")
write.csv(altPA.public_count_S7[c(3,5,6,2,4,1),], "effectdirection_public_altPA_S7.csv")
write.csv(altPA.private_count_S7[c(3,5,6,2,4,1),], "effectdirection_private_altPA_S7.csv")



# synthesis table 2: table that summarizes effect magnitude ----
# control_table= myprivate_table[which(myprivate_table$Treatment != "Communal"),]
# control= "private"
# consistentScales = T

get_ranks <- function(control_table, control, consistentScales, effect){
  
  # exclude N<40
  mydata <- control_table[which(control_table$N >= 40),]
  # deal with nonsignificant effects by replacing them with 0's
  if(effect == "ame"){
    eff_pos <- grep("Effect", colnames(mydata))
    p_pos <- grep("^p_value$", colnames(mydata))
  }
  if(effect == "tate"){
    eff_pos <- grep("TATE$", colnames(mydata))
    p_pos <- grep("TATE_p_value$", colnames(mydata))
  }
  mydata[,eff_pos][which(mydata[,p_pos] >= 0.05)] <- 0
  
  # round balance stats
  mydata$ImbBefore <- round(mydata$ImbBefore, digits=3)
  mydata$ImbAfter <- round(mydata$ImbAfter, digits=3)
  mydata$ImbImprov <- mydata$ImbBefore-mydata$ImbAfter
  # get spatial temporal scale in one column
  mydata <- unite(mydata, col = sptemp_scale, c("spatialScale","temporalScale"), sep = "_", remove = F)
  # universally exclude brazil 1985-2018 bc we only want narrow scales
  mydata <- mydata[which(mydata$sptemp_scale != "Brazil_1985-2018"),] 
  # universally exclude pampa and pantanal because a ranking of scales doesn't make sense here - where there is only ONE model per scale
  mydata <- mydata[which(mydata$spatialScale != "Pantanal"),]
  mydata <- mydata[which(mydata$spatialScale != "Pampa"),]
  
  # exclude other specific scales for the scale-consistent versions of this table
  if(consistentScales==T){
    
    # before anything else, I need to exclude the row of sustainable use vs. private - Caatinga 1985-2018, because we weren't able to estimate it at all at. 
    # rendering this a non-consistent scale.
    if(effect == "ame"){
      eff_pos <- grep("Effect", colnames(mydata))
      p_pos <- grep("^p_value$", colnames(mydata))
    }
    if(effect == "tate"){
      eff_pos <- grep("TATE$", colnames(mydata))
      p_pos <- grep("TATE_p_value$", colnames(mydata))
    }
    mydata$rm <- NA
    mydata$rm <- ifelse(is.na(mydata[,p_pos]),T, F)# specify the if condition to be sensitive to p-values being NA - i.e. a model that wasn't calculated
    mydata <- mydata[which(mydata$rm != T),]
    
    # first get scales that are needed
    commonDenominators <- list()
    for(i in 1:length(unique(mydata$Treatment))) 
    {
      commonDenominators[[i]] <- getLCD(mydata, treatment = unique(mydata$Treatment)[i])
    }
    # Reduce, and intersect calculates where all these scales intercept!
    myScales <- Reduce(intersect, commonDenominators)
    print(length(myScales))
    
    mydata <- inner_join(mydata, data.frame("sptemp_scale"=myScales), by="sptemp_scale")
  }
  
  mydata <- split(mydata, factor(mydata$sptemp_scale))
  length(mydata) 
  
  # for every scale add a column with the ranking of best to worst:
  for(i in 1:length(mydata))
  {
    # rank effect sizes in ascending order(most neg to most pos). Most negative to most positive effects means 1=best, 5=worst for forests
    mydata[[i]]$rank <- rank(mydata[[i]][,eff_pos], na.last="keep")
    # best and worst columns
    mydata[[i]]$best <- 0
    mydata[[i]]$worst <- 0
    # best == 0-1 
    mydata[[i]]$best[which(mydata[[i]]$rank == min(mydata[[i]]$rank, na.rm=T))] <- 1
    best_length <- sum(mydata[[i]]$rank == min(mydata[[i]]$rank))
    # if there are ties, (ties happen in private comparisons where several nonsig and decreasing effect)
    if(best_length > 1){
      mydata[[i]]$best[which(mydata[[i]]$rank == min(mydata[[i]]$rank, na.rm=T))] <- 1/best_length
      # this makes it share the proportion of the time it was the worst at this scale
    }
    # worst == 0-1 
    mydata[[i]]$worst[which(mydata[[i]]$rank == max(mydata[[i]]$rank, na.rm=T))] <- 1
    worst_length <- sum(mydata[[i]]$rank == max(mydata[[i]]$rank))
    # if there are ties for the worst place
    if(worst_length > 1){
      mydata[[i]]$worst[which(mydata[[i]]$rank == max(mydata[[i]]$rank, na.rm=T))] <- 1/worst_length
      # this makes it share the proportion of the time it was the worst at this scale
    }
    
    # re-calculate ranking but weighing the ranks themselves (NOT the effect)
    # add a column for the worst balance at this scale
    mydata[[i]]$worstBal <- 1-(max(mydata[[i]]$ImbAfter)) # transform worst remaining imbalance at this specific scale to worst balance measure
    # add weighted ranks
    mydata[[i]]$w.best <- mydata[[i]]$best*mydata[[i]]$worstBal
    mydata[[i]]$w.worst <- mydata[[i]]$worst*mydata[[i]]$worstBal
  }
  # put all data together again
  mydata2 <- do.call("rbind", unname(mydata))
  
  # split into a list by the tenure comparisons made
  mydata2 <- split(mydata2, factor(mydata2$Treatment))
  # for every comparison
  ranking <- list()
  for(i in 1:length(mydata2))
  {
    # find the % of time it was the best
    best <- sum(mydata2[[i]]$best)/nrow(mydata2[[i]]) 
    # find the % of the time it was the worst 
    worst <- sum(mydata2[[i]]$worst)/nrow(mydata2[[i]]) 
    
    # find the % of time it was the best in the weighted ranks
    w.best <- sum(mydata2[[i]]$w.best)/sum(mydata2[[i]]$worstBal) 
    # find the % of the time it was the worst in the weighted ranks
    w.worst <- sum(mydata2[[i]]$w.worst)/sum(mydata2[[i]]$worstBal)
    
    # these sums of best worst places reflect a proportion of 0-1 when the worst place is a tie - and is shared between 2-4 models
    # the weighted %'s reflect the sum of the weighted cases/total amount of possible (minimum) balance at evaluated cases 
    
    # add pvalue info
    mydata2[[i]]$nonsig <- 0
    if(nrow(mydata2[[i]][which(mydata2[[i]]$p_value >= 0.05),])>0){
      mydata2[[i]][which(mydata2[[i]]$p_value >= 0.05),]$nonsig <- 1
    }
    nonsig <- sum(mydata2[[i]]$nonsig)
    w.nonsig <- sum(mydata2[[i]]$nonsig*mydata2[[i]]$worstBal)
    
    Tbal <- sum(mydata2[[i]]$worstBal)
    n_mod <- nrow(mydata2[[i]])
    # create df with %'s
    ranking[[i]] <- data.frame("treatmt" = paste0(names(mydata2[i])), "best"=best, "w.best"=w.best, "worst"= worst, "w.worst"=w.worst, 
                               "nonsig"=nonsig,  "w.nonsig"=w.nonsig, "n_mod"=n_mod, "Tbal"=Tbal)
  }
  ranking
  rankingtbl <- do.call("rbind", unname(ranking))
  rankingtbl
  
  return(rankingtbl)
}

# calculate main ranks
# (for main ranks, specify we exclude communal. So we can ask for full ranks in Amazonia later)
vspublic_ranks <- get_ranks(control_table = mypublic_table[which(mypublic_table$Treatment != "Communal"),], control="public", consistentScales = T, effect = "ame") 
vspublic_ranks[c(2,3,5,1,4),]
vsprivate_ranks <- get_ranks(myprivate_table[which(myprivate_table$Treatment != "Communal"),], control="private", consistentScales = T, effect = "ame")
vsprivate_ranks[c(5,2,4,1,3),]
# write S5
write.csv(vspublic_ranks[c(2,3,5,1,4),], "effectrank_public.csv")
write.csv(vsprivate_ranks[c(5,2,4,1,3),], "effectrank_private.csv")

altPA.vspublic_ranks <- get_ranks(mypublic_table_altPA[which(mypublic_table_altPA$Treatment != "Communal"),], control="public", consistentScales = T, effect = "ame") 
altPA.vspublic_ranks[c(2,3,5,1,4),]
altPA.vsprivate_ranks <- get_ranks(myprivate_table_altPA[which(myprivate_table_altPA$Treatment != "Communal"),], control="private", consistentScales = T, effect = "ame")
altPA.vsprivate_ranks[c(5,2,4,1,3),]
# write S6
write.csv(altPA.vspublic_ranks[c(2,3,5,1,4),], "effectrank_public_altPA.csv")
write.csv(altPA.vsprivate_ranks[c(5,2,4,1,3),], "effectrank_private_altPA.csv")














