# ****************************** #
# Meta-analyses summarizing effects 
# ****************************** #
library(metafor)
library(tidyr)
library(dplyr)

wdmain <- "/data/MAS-group-share/04_personal/Andrea/P1_analysis/outputs/"
wd_sum <- paste0(wdmain, "model_summaries/")
wd_plot <- paste0(wdmain, "plots/forestplots")

# read in data
setwd(wd_sum)
vspublic <- read.csv("all_models_vspublic.csv")
vsprivate <- read.csv("all_models_vsprivate.csv")

# set up data ----

# public control
public_table <- vspublic[,-1]
public_table <- public_table[which(public_table$N >= 40),] # only interpret datasets larger than N=40
# convert to percentages
public_table$Effect <- public_table$Effect*100
public_table$SE <- public_table$SE*100
# create new column merging spatial and temporal scales
public_table <- unite(public_table, col = sptemp_scale, c("spatialScale","temporalScale"), sep = " ", remove = F)
# calculate sampling variance needed for meta analysis
public_table$var <- (public_table$SE^2) * (public_table$N-1) # subtracting -1 is common practice to calc the actual population sample
# create new column defining subgroups within 
public_table$subgr <- "D" 
public_table[which(public_table$spatialScale != "Brazil" & public_table$temporalScale != "1985-2018"),]$subgr <- "A" 
public_table[which(public_table$spatialScale != "Brazil" & public_table$temporalScale == "1985-2018"),]$subgr <- "C" 
public_table[which(public_table$spatialScale == "Brazil" & public_table$temporalScale != "1985-2018"),]$subgr <- "B" 
# split into individual lists per treatment
public_models <- split(public_table, factor(public_table$Treatment))
lapply(public_models, head)
for(i in 1:length(public_models))
{
  public_models[[i]] <- arrange(arrange(arrange(public_models[[i]], desc(temporalScale)),desc(spatialScale)), subgr)
}
lapply(public_models, tail)

#private control
private_table <- vsprivate[,-1]
private_table <- private_table[which(private_table$N >= 40),]
# convert to percentages
private_table$Effect <- private_table$Effect*100
private_table$SE <- private_table$SE*100
# create new column merging spatial and temporal scales
private_table <- unite(private_table, col = sptemp_scale, c("spatialScale","temporalScale"), sep = " ", remove = F)
# calculate sampling variance needed for meta analysis
private_table$var <- (private_table$SE^2) * (private_table$N-1) 
# create new column defining subgroups within 
private_table$subgr <- "D" 
private_table[which(private_table$spatialScale != "Brazil" & private_table$temporalScale != "1985-2018"),]$subgr <- "A" 
private_table[which(private_table$spatialScale != "Brazil" & private_table$temporalScale == "1985-2018"),]$subgr <- "C" 
private_table[which(private_table$spatialScale == "Brazil" & private_table$temporalScale != "1985-2018"),]$subgr <- "B" 
# split into individual lists per treatment
private_models <- split(private_table, factor(private_table$Treatment))
lapply(private_models, head)
for(i in 1:length(private_models))
{
  private_models[[i]] <- arrange(arrange(arrange(private_models[[i]], desc(temporalScale)),desc(spatialScale)), subgr)
}
lapply(private_models, tail)

# meta analyses ----

# create function(s) for entire meta-analysis
MA_main <- function(datalist){
  MA <- list()
  for(i in 1:length(datalist))
  {
    MA[[i]] <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # measure of variance we've created
                 random = list(~1|spatialScale, ~1|temporalScale), # spatial and temporal scales as random effects
                 data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, )
  }
  names(MA) <- names(datalist)
  return(MA)
}
mypublic_MA <- MA_main(public_models)
myprivate_MA <- MA_main(private_models)

# create forest plots ----
fplot_MA <- function(myMA, datalist, myname){
  # main forest plot of main MA
  setwd(wd_plot)
  for(i in 1:length(myMA)) # for every meta-analysis:
  {
    # patterns of numbers of "observations" (our models) within our subgroups: 
    # main pattern: A=24, B=6, C=4, D=1
    # exceptions:
    # D=1, C=1, B=6, A=6
    # D=1, C=3, B=6, A=18
    # D=1, C=6, B=6, A=36
    # one more: A=24, B=6, C=4, D=1
    
    # create series of if conditions for creating forest plots for each of these unique numbers of rows needed in the forest plots
    
    #main:
    if(nrow(datalist[[i]][which(datalist[[i]]$subgr == "A"),]) == 24){
      if(is.na(datalist[[i]][33,]$Effect)){
        png(file = paste0("MA_",myname, "_", names(myMA)[i],".png"), width = 2550, height = 3500, units = "px", res = 300)
        
        forest(myMA[[i]], order=as.vector(1:nrow(datalist[[i]])), cex=1, 
               rows=c(3:26,30:35,39:42,45), ylim=c(-1, 50), alim = c(-30,10), xlim = c(-70,30),
               header = c("Spatial-Temporal scale", "W    Estimate (95% CI)"), mlab = "", xlab = "Average Marginal Effects (AME)",
               psize=1, col = "gray70", border = F)
        text(-70, -1, pos=4, cex=1, col = "black", bquote(paste("RE model for all models (Q = ",
                                                                .(formatC(myMA[[i]]$QE, digits=2, format="f")), ", df = ", .(myMA[[i]]$k - myMA[[i]]$p),
                                                                ", p = ", .(formatC(myMA[[i]]$QEp, digits=2, format="f")), ")")))
        op <- par(cex=.8, font=4)
        text(-70, c(27,36,42), pos=4, c("Biomes small temporal extents", "Brazil small temporal extents", "Biomes 1985-2018"))
        
        # calculate models specific to subgroups
        myMA1 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                        random = list(~1|spatialScale), # this includes spatial and temporal scales as random effects
                        data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "C")
        
        myMA2 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                        random = list(~1|temporalScale), # this includes spatial and temporal scales as random effects
                        data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "B")
        
        myMA3 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                        random = list(~1|spatialScale, ~1|temporalScale), # this includes spatial and temporal scales as random effects
                        data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "A")
        
        addpoly(myMA1, row = 37.5, cex=1, mlab="", col = "gray70", border=F)
        addpoly(myMA2, row = 28.5, cex=1, mlab="", col = "gray70", border=F)
        addpoly(myMA3, row = 1.5, cex=1, mlab="", col = "gray70", border=F)
        
        
        text(-70, 37.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                                  .(formatC(myMA1$QE, digits=2, format="f")), ", df = ", .(myMA1$k - myMA1$p),
                                                                  ", p = ", .(formatC(myMA1$QEp, digits=2, format="f")), ")")))
        text(-70, 28.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                                  .(formatC(myMA2$QE, digits=2, format="f")), ", df = ", .(myMA2$k - myMA2$p),
                                                                  ", p = ", .(formatC(myMA2$QEp, digits=2, format="f")), ")")))
        text(-70, 1.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                                 .(formatC(myMA3$QE, digits=2, format="f")), ", df = ", .(myMA3$k - myMA3$p),
                                                                 ", p = ", .(formatC(myMA3$QEp, digits=2, format="f")), ")")))
        dev.off()
      }else{
        png(file = paste0("MA_",myname, "_", names(myMA)[i],".png"), width = 2550, height = 3500, units = "px", res = 300)
        
        forest(myMA[[i]], order=as.vector(1:nrow(datalist[[i]])), cex=1, 
               rows=c(3:26,30:35,39:42,45), ylim=c(-1, 50), alim = c(-30,10), xlim = c(-70,35),
               header = c("Spatial-Temporal scale", "W    Estimate (95% CI)"), mlab = "", xlab = "Average Marginal Effects (AME)",
               psize=1, col = "gray70", border = F)
        text(-70, -1, pos=4, cex=1, col = "black", bquote(paste("RE model for all models (Q = ",
                                                                .(formatC(myMA[[i]]$QE, digits=2, format="f")), ", df = ", .(myMA[[i]]$k - myMA[[i]]$p),
                                                                ", p = ", .(formatC(myMA[[i]]$QEp, digits=2, format="f")), ")")))
        op <- par(cex=.8, font=4)
        text(-70, c(27,36,43), pos=4, c("Biomes small temporal extents", "Brazil small temporal extents", "Biomes 1985-2018"))
        
        # calculate models specific to subgroups
        myMA1 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                        random = list(~1|spatialScale), # this includes spatial and temporal scales as random effects
                        data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "C")
        
        myMA2 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                        random = list(~1|temporalScale), # this includes spatial and temporal scales as random effects
                        data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "B")
        
        myMA3 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                        random = list(~1|spatialScale, ~1|temporalScale), # this includes spatial and temporal scales as random effects
                        data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "A")
        
        addpoly(myMA1, row = 37.5, cex=1, mlab="", col = "gray70", border=F)
        addpoly(myMA2, row = 28.5, cex=1, mlab="", col = "gray70", border=F)
        addpoly(myMA3, row = 1.5, cex=1, mlab="", col = "gray70", border=F)
        
        
        text(-70, 37.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                                  .(formatC(myMA1$QE, digits=2, format="f")), ", df = ", .(myMA1$k - myMA1$p),
                                                                  ", p = ", .(formatC(myMA1$QEp, digits=2, format="f")), ")")))
        text(-70, 28.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                                  .(formatC(myMA2$QE, digits=2, format="f")), ", df = ", .(myMA2$k - myMA2$p),
                                                                  ", p = ", .(formatC(myMA2$QEp, digits=2, format="f")), ")")))
        text(-70, 1.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                                 .(formatC(myMA3$QE, digits=2, format="f")), ", df = ", .(myMA3$k - myMA3$p),
                                                                 ", p = ", .(formatC(myMA3$QEp, digits=2, format="f")), ")")))
        dev.off()
      }
      
    }
  
    # exception 1:
    if(nrow(datalist[[i]][which(datalist[[i]]$subgr == "A"),]) == 6){ 

      png(file = paste0("MA_",myname, "_", names(myMA)[i],".png"), width = 2550, height = 3500, units = "px", res = 300)

      forest(myMA[[i]], order=as.vector(1:nrow(datalist[[i]])), cex=1, 
             rows=c(3:8,13:18,21,23), ylim=c(-1, 26), alim = c(-20,5), xlim = c(-70,30),
             header = c("Spatial-Temporal scale", "W    Estimate (95% CI)"), mlab = "", xlab = "Average Marginal Effects (AME)",
             psize=1, col = "gray70", border = F)
      text(-70, -1, pos=4, cex=1, col = "black", bquote(paste("RE model for all models (Q = ",
                                                              .(formatC(myMA[[i]]$QE, digits=2, format="f")), ", df = ", .(myMA[[i]]$k - myMA[[i]]$p),
                                                              ", p = ", .(formatC(myMA[[i]]$QEp, digits=2, format="f")), ")")))
      op <- par(cex=.8, font=4)
      text(-70, c(9,19), pos=4, c("Biomes small temporal extents", "Brazil small temporal extents"))
      
      # calculate models specific to subgroups
      myMA1 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                      random = list(~1|temporalScale), # this includes spatial and temporal scales as random effects
                      data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "B")
      
      myMA2 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                      random = list(~1|temporalScale), # this includes spatial and temporal scales as random effects
                      data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "A")
      
      addpoly(myMA1, row = 11.5, cex=1, mlab="", col = "gray70", border=F)
      addpoly(myMA2, row = 1.5, cex=1, mlab="", col = "gray70", border=F)

      text(-70, 11.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                              .(formatC(myMA1$QE, digits=2, format="f")), ", df = ", .(myMA1$k - myMA1$p),
                                                              ", p = ", .(formatC(myMA1$QEp, digits=2, format="f")), ")")))
      text(-70, 1.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                                .(formatC(myMA2$QE, digits=2, format="f")), ", df = ", .(myMA2$k - myMA2$p),
                                                                ", p = ", .(formatC(myMA2$QEp, digits=2, format="f")), ")")))
      dev.off()

    }
    # exception 2:
    if(nrow(datalist[[i]][which(datalist[[i]]$subgr == "A"),]) == 18){
      
      png(file = paste0("MA_",myname, "_", names(myMA)[i],".png"), width = 2550, height = 3500, units = "px", res = 300)
      
      forest(myMA[[i]], order=as.vector(1:nrow(datalist[[i]])), cex=1, 
             rows=c(3:20,24:29,33:35,39), ylim=c(-1, 42), alim = c(-40,5), xlim = c(-70,30),
             header = c("Spatial-Temporal scale", "W    Estimate (95% CI)"), mlab = "", xlab = "Average Marginal Effects (AME)",
             psize=1, col = "gray70", border = F)
      text(-70, -1, pos=4, cex=1, col = "black", bquote(paste("RE model for all models (Q = ",
                                                              .(formatC(myMA[[i]]$QE, digits=2, format="f")), ", df = ", .(myMA[[i]]$k - myMA[[i]]$p),
                                                              ", p = ", .(formatC(myMA[[i]]$QEp, digits=2, format="f")), ")")))
      op <- par(cex=.8, font=4)
      text(-70, c(21,30,36), pos=4, c("Biomes small temporal extents", "Brazil small temporal extents", "Biomes 1985-2018"))
      
      # calculate models specific to subgroups
      myMA1 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                      random = list(~1|spatialScale), # this includes spatial and temporal scales as random effects
                      data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "C")
      
      myMA2 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                      random = list(~1|temporalScale), # this includes spatial and temporal scales as random effects
                      data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "B")
      
      myMA3 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                      random = list(~1|spatialScale, ~1|temporalScale), # this includes spatial and temporal scales as random effects
                      data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "A")
      
      addpoly(myMA1, row = 31.5, cex=1, mlab="", col = "gray70", border=F)
      addpoly(myMA2, row = 22.5, cex=1, mlab="", col = "gray70", border=F)
      addpoly(myMA3, row = 1.5, cex=1, mlab="", col = "gray70", border=F)
      
      
      text(-70, 31.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                                .(formatC(myMA1$QE, digits=2, format="f")), ", df = ", .(myMA1$k - myMA1$p),
                                                                ", p = ", .(formatC(myMA1$QEp, digits=2, format="f")), ")")))
      text(-70, 22.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                                .(formatC(myMA2$QE, digits=2, format="f")), ", df = ", .(myMA2$k - myMA2$p),
                                                                ", p = ", .(formatC(myMA2$QEp, digits=2, format="f")), ")")))
      text(-70, 1.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                               .(formatC(myMA3$QE, digits=2, format="f")), ", df = ", .(myMA3$k - myMA3$p),
                                                               ", p = ", .(formatC(myMA3$QEp, digits=2, format="f")), ")")))
      dev.off()
    }
    # exception 3:
    if(nrow(datalist[[i]][which(datalist[[i]]$subgr == "A"),]) == 36){
      if(names(datalist)[i] == "Public"){
        
        png(file = paste0("MA_",myname, "_", names(myMA)[i],".png"), width = 2550, height = 3500, units = "px", res = 300)
        
        forest(myMA[[i]], order=as.vector(1:nrow(datalist[[i]])), cex=1, 
               rows=c(3:38,43:48,53:58,61), ylim=c(-1, 65), alim = c(-10,25), xlim = c(-60,50),
               header = c("Spatial-Temporal scale", "W    Estimate (95% CI)"), mlab = "", xlab = "Average Marginal Effects (AME)",
               psize=1, col = "gray70", border = F)
        text(-60, -1, pos=4, cex=1, col = "black", bquote(paste("RE model for all models (Q = ",
                                                                .(formatC(myMA[[i]]$QE, digits=2, format="f")), ", df = ", .(myMA[[i]]$k - myMA[[i]]$p),
                                                                ", p = ", .(formatC(myMA[[i]]$QEp, digits=2, format="f")), ")")))
        op <- par(cex=.8, font=4)
        text(-60, c(39,49,59), pos=4, c("Biomes small temporal extents", "Brazil small temporal extents", "Biomes 1985-2018"))
        
        # calculate models specific to subgroups
        myMA1 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                        random = list(~1|spatialScale), # this includes spatial and temporal scales as random effects
                        data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "C")
        
        myMA2 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                        random = list(~1|temporalScale), # this includes spatial and temporal scales as random effects
                        data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "B")
        
        myMA3 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                        random = list(~1|spatialScale, ~1|temporalScale), # this includes spatial and temporal scales as random effects
                        data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "A")
        
        addpoly(myMA1, row = 51.5, cex=1, mlab="", col = "gray70", border=F)
        addpoly(myMA2, row = 41.5, cex=1, mlab="", col = "gray70", border=F)
        addpoly(myMA3, row = 1.5, cex=1, mlab="", col = "gray70", border=F)
        
        
        text(-60, 51.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                                  .(formatC(myMA1$QE, digits=2, format="f")), ", df = ", .(myMA1$k - myMA1$p),
                                                                  ", p = ", .(formatC(myMA1$QEp, digits=2, format="f")), ")")))
        text(-60, 41.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                                  .(formatC(myMA2$QE, digits=2, format="f")), ", df = ", .(myMA2$k - myMA2$p),
                                                                  ", p = ", .(formatC(myMA2$QEp, digits=2, format="f")), ")")))
        text(-60, 1.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                                 .(formatC(myMA3$QE, digits=2, format="f")), ", df = ", .(myMA3$k - myMA3$p),
                                                                 ", p = ", .(formatC(myMA3$QEp, digits=2, format="f")), ")")))
        dev.off()
      }else{
        
        png(file = paste0("MA_",myname, "_", names(myMA)[i],".png"), width = 2550, height = 3500, units = "px", res = 300)
        
        forest(myMA[[i]], order=as.vector(1:nrow(datalist[[i]])), cex=1, 
               rows=c(3:38,43:48,53:58,61), ylim=c(-1, 65), alim = c(-25,10), xlim = c(-70,30),
               header = c("Spatial-Temporal scale", "W    Estimate (95% CI)"), mlab = "", xlab = "Average Marginal Effects (AME)",
               psize=1, col = "gray70", border = F)
        text(-70, -1, pos=4, cex=1, col = "black", bquote(paste("RE model for all models (Q = ",
                                                                .(formatC(myMA[[i]]$QE, digits=2, format="f")), ", df = ", .(myMA[[i]]$k - myMA[[i]]$p),
                                                                ", p = ", .(formatC(myMA[[i]]$QEp, digits=2, format="f")), ")")))
        op <- par(cex=.8, font=4)
        text(-70, c(39,49,59), pos=4, c("Biomes small temporal extents", "Brazil small temporal extents", "Biomes 1985-2018"))
        
        # calculate models specific to subgroups
        myMA1 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                        random = list(~1|spatialScale), # this includes spatial and temporal scales as random effects
                        data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "C")
        
        myMA2 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                        random = list(~1|temporalScale), # this includes spatial and temporal scales as random effects
                        data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "B")
        
        myMA3 <- rma.mv(datalist[[i]]$Effect, datalist[[i]]$var, # includes the measure of variance we've created
                        random = list(~1|spatialScale, ~1|temporalScale), # this includes spatial and temporal scales as random effects
                        data = datalist[[i]], tdist = T, method = "REML", slab = datalist[[i]]$sptemp_scale, subset = datalist[[i]]$subgr == "A")
        
        addpoly(myMA1, row = 51.5, cex=1, mlab="", col = "gray70", border=F)
        addpoly(myMA2, row = 41.5, cex=1, mlab="", col = "gray70", border=F)
        addpoly(myMA3, row = 1.5, cex=1, mlab="", col = "gray70", border=F)
        
        
        text(-70, 51.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                                  .(formatC(myMA1$QE, digits=2, format="f")), ", df = ", .(myMA1$k - myMA1$p),
                                                                  ", p = ", .(formatC(myMA1$QEp, digits=2, format="f")), ")")))
        text(-70, 41.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                                  .(formatC(myMA2$QE, digits=2, format="f")), ", df = ", .(myMA2$k - myMA2$p),
                                                                  ", p = ", .(formatC(myMA2$QEp, digits=2, format="f")), ")")))
        text(-70, 1.5, pos=4, cex=1, col = "black", bquote(paste("RE model (Q = ",
                                                                 .(formatC(myMA3$QE, digits=2, format="f")), ", df = ", .(myMA3$k - myMA3$p),
                                                                 ", p = ", .(formatC(myMA3$QEp, digits=2, format="f")), ")")))
        dev.off()
      }
      
    }

  }
}

fplot_MA(mypublic_MA, public_models, "public")
fplot_MA(myprivate_MA, private_models, "private")





