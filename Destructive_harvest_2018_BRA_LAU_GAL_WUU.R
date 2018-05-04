#### 
# This code is to analyse the 2018 field data from destructive harvests
# written by Miro

remove(list=ls())

# load packages and functions ----
    delete.na <- function(DF, n=0) {
      DF[rowSums(is.na(DF)) <= n,]
    }



# read data, check data ----
  setwd('C:/Users/midemol/Dropbox/Doctoraat/fun_in_R')
  inventory = read.csv('inventory_BRA_LAU_WUU_GAL.csv')
  str(inventory)
  # clean up inventory
  inventory=inventory[!(inventory$Tree_Code=='GAL-12'), ]           # remove GAL12
  inventory$site_code=factor(substring(inventory$Tree_Code,1,3))    # make site code factors (4 factor levels, one per site)
  levels(inventory$Tree_height_felled_flb_.m.)[levels(inventory$Tree_height_felled_flb_.m.)=="9,3 / 7,6"] <- "9.3"  # forking tree. keep only max height of flb for now.
  inventory$Tree_height_felled_flb_.m.=as.numeric(as.character(inventory$Tree_height_felled_flb_.m.))
  # the dead braches of WUU-01 are not in the main database, we add them here:
    id=c('pom','tien1','tien2');fresh_volume=c(5050,1084,3235);fresh_mass=c(5501,797,2814);dry_mass=c(2787,501,1540);WUU01=data.frame(id,fresh_volume,fresh_mass,dry_mass)
  
  # add columns
    inventory$WSG_coreA=inventory$coreA_dry_mass_.g./inventory$coreA_fresh_volume_.mL.
    inventory$WSG_coreB=inventory$coreB_dry_mass_.g./inventory$coreB_fresh_volume_.mL.
    inventory$WSG_cores=(inventory$WSG_coreA+inventory$WSG_coreB)/2
    
    
# exploratory analyses & descriptive statistics ----
  summary(inventory)
  
  attach(inventory)
    mean_dbh=mean(DBH)
    mean_th=mean(Tree_height_felled_.m.)
    
    aggregate(Tree_height_felled_.m., list(site=site_code), FUN=function(x) c(mean(x,na.rm = T), min(x,na.rm = T), max(x,na.rm = T)))
    par(pty="s")
    plot(Circumference_standing_.cm.,Circ_felled_.cm.,col=site_code,pch=c(0,1,2,3)[site_code])
    plot(Tree_height_felled_.m.,Tree_height_.m.,col=site_code,pch=c(0,1,2,3)[site_code],xlim = c(15,26),ylim = c(15,26),pty='s');abline(0,1)

    plot(Tree_height_felled_.m.,Tree_height_felled_flb_.m.,col=site_code,pch=c(0,1,2,3)[site_code],pty='s')
    
    #pdf("C:/Users/midemol/Dropbox/Doctoraat/fun_in_R/figures/DBH2TH_biomass.pdf",4,4)
        par(mar = c(4, 4, 1, 0.2))
    plot(Tree_height_felled_.m.,Crown_weight_.kg. + Stem_weight_.kg.,col=site_code,pch=c(0,1,2,3)[site_code],pty='s',xlab = 'Total tree length (m)',ylab = 'Tree total fresh mass (kg)')
    plot(DBH,Crown_weight_.kg. + Stem_weight_.kg.,col=site_code,pch=c(0,1,2,3)[site_code],pty='s',xlab = 'DBH (cm)',ylab = 'Tree total fresh mass (kg)')
    with(inventory,plot(DBH^2*Tree_height_felled_.m.,Crown_weight_.kg. + Stem_weight_.kg.,xlab=expression(~ DBH^{2} ~ x~Tree ~length ~ (cm^{2} ~ m)),ylab='Tree total fresh mass (kg)',col=site_code,pch=c(0,1,2,3)[site_code]))
    
    legend('topleft',title='Site',c("BRA","GAL","LAU","WUU"), 
           col=seq_along(levels(factor(site_code))), pch=c(0,1,2,3),bty='n', cex=.75)
    dev.off
  detach(inventory) 
    # standing vs felled ----
    #pdf("C:/Users/midemol/Dropbox/Doctoraat/fun_in_R/figures/DBH_DBH.pdf",4,4)
    #pdf("C:/Users/midemol/Dropbox/Doctoraat/fun_in_R/figures/TH_TH.pdf",4,4)
    #pdf("C:/Users/midemol/Dropbox/Doctoraat/fun_in_R/figures/THflb_THflb.pdf",4,4)
    
    
    par(mar = c(4, 4, 1, 1))
    with(inventory,plot(Circumference_standing_.cm.,Circumference_standing_.cm.-Circ_felled_.cm.,xlab='Circumference of standing tree (cm)',ylab='Difference (standing - felled, cm)',col=site_code,pch=c(0,1,2,3)[site_code]))
        abline(h = 0, lty = 2)
    with(inventory,plot(Tree_height_.m.,Tree_height_.m.- Tree_height_felled_.m.,xlab='Forestry Pro tree height (m)',ylab='Difference (Forestry Pro - felled, m)',col=site_code,pch=c(0,1,2,3)[site_code]))
        abline(h = 0, lty = 2)    
    with(inventory,plot(Tree_height_flb_.m.,Tree_height_flb_.m.- Tree_height_felled_flb_.m.,xlab='Forestry Pro tree height till first living branch (m)',ylab='Difference (Forestry Pro - felled, m)',col=site_code,pch=c(0,1,2,3)[site_code]))
        abline(h = 0, lty = 2)
    dev.off()    
    
    
    # wood cores and wood density ----
    with(inventory,plot(DBH,WSG_cores,xlab='DBH (cm)',ylab='WSG cores',col=site_code,pch=c(0,1,2,3)[site_code]))
    with(inventory,plot(DBH,Stem_disks_fresh_mass_.g._POM/Stem_disks_fresh_volume_.ml._POM,xlab='DBH (cm)',ylab='Fresh disc density',col=site_code,pch=c(0,1,2,3)[site_code]))
    with(inventory,plot(DBH,Dry_mass_POM../Stem_disks_fresh_volume_.ml._POM,xlab='DBH (cm)',ylab='Fresh disc density',col=site_code,pch=c(0,1,2,3)[site_code]))
    with(inventory,plot(WSG_coreA,abs(WSG_coreA- WSG_coreB),xlab='WSG core A',ylab='WSG core A - WSG core B',col=site_code,pch=c(0,1,2,3)[site_code]));        abline(h = 0, lty = 2)    
    boxplot(WSG_cores ~ site_code, data = inventory,ylab='WSG from cores (g/cu. cm)')
    
    # dead wood from WUU01 (WSG=wood specific gravity, dmc=dry matter content, FWD=fresh wood density)
    WUU01$WSG=WUU01$dry_mass/WUU01$fresh_volume;WUU01$DMC=WUU01$dry_mass/WUU01$fresh_mass;WUU01$FWD=WUU01$fresh_mass/WUU01$fresh_volume
    
    
    #pdf("C:/Users/midemol/Dropbox/Doctoraat/fun_in_R/figures/fresh_biomass_boxplot.pdf",4,4)
      par(mar = c(3, 4, 1, 1))
    boxplot(DBH ~ site_code, data = inventory,ylab='DBH (cm)')
    boxplot(Tree_height_felled_.m. ~ site_code, data = inventory,ylab='Tree length (m)')
    boxplot(Stem_weight_.kg. ~ site_code, data = inventory)
    boxplot(Crown_weight_.kg. ~ site_code, data = inventory)
    boxplot(Crown_weight_.kg. + Stem_weight_.kg.~ site_code, data = inventory,ylab='Tree total fresh mass (kg)')
    dev.off()
  # linear regressions  
    m <- lm(log(Crown_weight_.kg. + Stem_weight_.kg.)~I(log(DBH)),data=inventory)
      #plot(m,which=1:2)
      
    
      
    # WD and water content at different heights ----
      #make a dataframe "discs" with columns (height, site code, fresh mass and volume and dry mass)
      discs=data.frame(130,inventory$site_code,inventory$Stem_disks_fresh_mass_.g._POM,inventory$Stem_disks_fresh_volume_.ml._POM,inventory$Dry_mass_POM..)
      names(discs)=c('h','site_code','fresh_mass','fresh_volume','dry_mass')
      
      for (i in c(3,6,9,12,15,18,21,24)){
        discs_temp=data.frame(i*100,inventory$site_code,inventory[[paste('Fresh_mass_POM.',i,'m',sep = '')]],inventory[[paste('Fresh_volume_POM.',i,'m',sep = '')]],inventory[[paste('Dry_mass_POM.',i,'m',sep = '')]])
          names(discs_temp)=c('h','site_code','fresh_mass','fresh_volume','dry_mass')
          discs=rbind(discs,discs_temp)
          rm(discs_temp)
      }
      
      #discs0=delete.na(discs)
      discs=delete.na(discs,n=1) # max number of NA in discs per row is n=1
      #discs2=delete.na(discs,n=2) # max number of NA in discs per row is n=2
      
      # calculate wsg etc in discs
      discs$WSG=discs$dry_mass/discs$fresh_volume;discs$DMC=discs$dry_mass/discs$fresh_mass;discs$FWD=discs$fresh_mass/discs$fresh_volume
      
      # plot the results!
      with(discs,plot(WSG,h,xlab='WSG',ylab='Height of disc (cm)',col=site_code,pch=c(0,1,2,3)[site_code]))
      with(discs,plot(DMC,h,xlab='DMC',ylab='Height of disc (cm)',col=site_code,pch=c(0,1,2,3)[site_code]))
      with(discs,plot(FWD,h,xlab='FWD',ylab='Height of disc (cm)',col=site_code,pch=c(0,1,2,3)[site_code]))
              legend('topleft',title='Site',c("BRA","GAL","LAU","WUU"), 
                         col=seq_along(levels(factor(discs$site_code))), pch=c(0,1,2,3),bty='n', cex=.75)
                  