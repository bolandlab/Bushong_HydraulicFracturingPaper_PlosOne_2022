#Author: Anna Bushong
#Date Compiled: 1/28/2022
#Last Modified: 03/09/2022

#Notes:
#1. In order to run this script, first download all
#data files under:
#https://github.com/bolandlab/Bushong_HydraulicFracturingPaper_PlosOne_2022/tree/main/Data
#2. Next set the fldr_dir variable to the file path
#where the downloaded folder is stored
#Any questions feel free to email: Dr. Mary Regina Boland at bolandm@upenn.edu


###############################################################
#                 Code for Data Analysis                      #
###############################################################
#Loading packages
library(dplyr)
library(ggplot2)

#Loading CSV files
##insert file path unique to user system
fldr_dir="/InsertFilePathHere"

#loading files
diff.d <- read.csv(paste0(fldr_dir,"/Data/diff_streamlined_07_28_2020.csv"))
MultiTest.d <- read.csv(paste0(fldr_dir,"/Data/multi.test.csv"))
HARaverages.d <- read.csv(paste0(fldr_dir,"/Data/AsthmaHAR_Yearly_Streamlined.csv"))


#Multicollinearity Evaluation
#pairs(MultiTest.d)

cor(MultiTest.d, use = "na.or.complete") #used to identify potential pairs of interest for further testing

##If cor.test() = ~0.5 (i.e. 0.49 investigated also), tested further
cor.test(MultiTest.d$m_income, MultiTest.d$perc_smoker, use = "na.or.complete") #significant

cor.test(MultiTest.d$m_income, MultiTest.d$peduc, use = "na.or.complete") #significant

cor.test(MultiTest.d$pUninsured, MultiTest.d$peduc, use = "na.or.complete") #significant

cor.test(MultiTest.d$perc_urban_pop, MultiTest.d$prw, use = "na.or.complete") #significant

#Rules for Multicollinearity##
  ##m_income cannot be in the same model as peduc & perc_smoker. 
  ##pUninsured cannot be in the same model as peduc
  ##perc_urban_pop cannot be in the same model as prw


#Wilcoxon sum rank for Urban v. Rural HAR analysis 
var.test(HARaverages.d$UrbanAvgRate, HARaverages.d$RuralAvgRate) #assumption met
wilcox.test(HARaverages.d$UrbanAvgRate, HARaverages.d$RuralAvgRate, alternative = "two.sided") 
##significant


#Filtering & Prepping for Regression Analysis

##Model 1 Filtering
diff.filter.all <- diff.d %>% 
  filter(year > 2004)
head(diff.filter.all)
diff.logasthma.filter.all <- log(diff.filter.all$asthma_rate)
year_factor_a <- as.factor(diff.filter.all$year_factor)

##Model 2 Filtering
diff.filter.2 <- diff.d %>% 
  filter(year > 2004) %>%
  filter(D_Rural == 1)
unique(diff.filter.2$year)
unique(diff.filter.2$D_Rural)
diff.logasthma.filter.2 <- log(diff.filter.2$asthma_rate)
year_factor_2 <-  as.factor(diff.filter.2$year_factor)


##Supplementary Model Filtering
lag1 <- diff.filter.2 %>%
  group_by(county) %>%
  mutate(PM25_1 = lag(PM25))
lag1_year_factor <- as.factor(lag1$year_factor)


#Coding for Primary Models 
  ##backtransformed coefficients via (exp(coefficient)-1)*100
  ##for every unit increase in coefficient, there is approx --% increase in asthma HAR

##Model 1 -- all counties
diff.log.step.py.lm.all.f <- lm(diff.logasthma.filter.all ~ py_well_density + m_income + perc_urban_pop + PM25 + year_factor_a, data=diff.filter.all) 
summary(diff.log.step.py.lm.all.f)
plot(diff.log.step.py.lm.all.f) #diagnostic plots for checking assumptions


##Model 1 -- recheck for multicollinearity between covariables 
cor.test(MultiTest.d$py_well_density, MultiTest.d$m_income, use = "na.or.complete")$estimate
cor.test(MultiTest.d$py_well_density, MultiTest.d$perc_urban_pop, use = "na.or.complete")$estimate
cor.test(MultiTest.d$py_well_density, MultiTest.d$PM25, use = "na.or.complete")$estimate
cor.test(MultiTest.d$m_income, MultiTest.d$PM25, use = "na.or.complete")$estimate
cor.test(MultiTest.d$m_income, MultiTest.d$perc_urban_pop, use = "na.or.complete")$estimate
cor.test(MultiTest.d$perc_urban_pop, MultiTest.d$PM25, use = "na.or.complete")$estimate

    ##calculating model error rate
    (sigma(diff.log.step.py.lm.all.f)/mean(diff.logasthma.filter.all, na.rm=TRUE))*100 #--> 16.07%
    
    ##interpreting per year well coefficient w/ years
    (exp(1.387e+00)-1)*100
    mean(diff.filter.all$py_well_density)
    300.2824*0.01 #you are multiplying the percent increase in asthma for an increase  1.0 well/sq mi increase. Not really helpful metric given range of per year, so you are multiplying that percent increase by 0.01 well/sq mi. 
    
    ##interpreting median income coefficient w/ years
    (exp(-1.464e-05)-1)*100
    1000*-0.001463989 #you are multiplying the percent per dollar by 1000 for the percent per thousand dollar decrease
    
    ##interpreting % pop urban coefficient w/ years
    (exp(8.648e-03)-1)*100
    
    ##interpreting PM2.5 coefficient w/ years
    (exp(2.669e-02)-1)*100
      
    ##Calculating 95% CIs -- Approach, calculate 95% CI from raw values, then backtransform to percent diff
      diff.log.step.py.lm.all.f$percent_change <- 100*(exp( diff.log.step.py.lm.all.f$coefficients)-1) #table with percent change for easy reference, note that per year well and median income are expressed in original per unit increase
      
      ##Approach, calculate CI from raw values, then back-transform bounds to percent diff
      ##py_well_density
      (1.387e+00) - (2*3.594e-01) #lower bound
      ((exp(0.6682)-1)*100)*0.01 #exp to percent change & change unit increase scale to 0.01 well/sq mi
      
      
      (1.387e+00) + (2*3.594e-01) #upper bound
      ((exp(2.1058)-1)*100)*0.01 #exp to percent change & change unit increase scale to 0.01 well/sq mi
      
      ##m_income
      (-1.464e-05) - (2*1.813e-06) #lower bound
      ((exp(-1.8266e-05)-1)*100)*1000 #exp to percent change & change unit increase scale to $1000
      
      (-1.464e-05) + (2*1.813e-06) # upper bound
      ((exp(-1.1014e-05)-1)*100)*1000 #exp to percent change & change unit increase scale to $1000
      
      ##% pop urban
      (8.648e-03) - (2*8.013e-04) #lower bound
      ((exp(0.0070454)-1)*100) #exp to percent change
      
      (8.648e-03) + (2*8.013e-04) #upper bound
      ((exp(0.0102506)-1)*100) #exp to percent change
      
      ##PM2.5
      (2.669e-02) - (2*1.212e-02) #lower bound
      ((exp(0.00245)-1)*100) #exp to percent change
      
      (2.669e-02) + (2*1.212e-02) #upper bound
      ((exp(0.05093)-1)*100) #exp to percent change


##Model 2 
diff.log.step.py.lm.years_factor <- lm(diff.logasthma.filter.2 ~ py_well_density + m_income + perc_rw + PM25 + year_factor_2, data=diff.filter.2) 
summary(diff.log.step.py.lm.years_factor)
plot(diff.log.step.py.lm.years_factor)

##Model 2 -- recheck for mullticollinearity between covariates 
cor.test(MultiTest.d$py_well_density, MultiTest.d$m_income, use = "na.or.complete")$estimate
cor.test(MultiTest.d$py_well_density, MultiTest.d$prw, use = "na.or.complete")$estimate
cor.test(MultiTest.d$py_well_density, MultiTest.d$PM25, use = "na.or.complete")$estimate
cor.test(MultiTest.d$m_income, MultiTest.d$prw, use = "na.or.complete")$estimate
cor.test(MultiTest.d$m_income, MultiTest.d$PM25, use = "na.or.complete")$estimate
cor.test(MultiTest.d$prw, MultiTest.d$PM25, use = "na.or.complete")$estimate
    
    ##calculating model error rate
    (sigma(diff.log.step.py.lm.years_factor)/mean(diff.logasthma.filter.2, na.rm=TRUE))*100 #--> 15.76%
    
    ##interpreting per year well coefficient w/ years
    (exp(1.462e+00)-1)*100
    mean(diff.filter.2$py_well_density)
    331.458*0.01818333
    331.458*0.01
    
    ##interpreting money coefficient w/ years
    (exp(-1.902e-05)-1)*100
    mean(diff.filter.2$m_income)
    1000*-0.001901982
    
    ##interpreting white coefficient w/ years
    (exp(-2.575e-02)-1)*100
    
    ##interpreting PM2.5 coefficient w/ years
    (exp(7.071e-02)-1)*100
  
  ##Calculating 95% CIs -- Approach, calculate 95% CI from raw values, then backtransform to percent diff
  diff.log.step.py.lm.years_factor$percent_change <- 100*(exp(diff.log.step.py.lm.years_factor$coefficients)-1) #table with percent change for easy reference, note that per year well and median income are expressed in original per unit increase

    ##Approach, calculate CI from raw values, then back-transform bounds to percent diff
    ##py_well_density
    (1.462e+00) - (2*3.505e-01) #lower bound
    ((exp(0.761)-1)*100)*0.01 #exp to percent change & change unit increase scale to 0.01 well/sq mi
    
    (1.462e+00) + (2*3.505e-01) #upper bound
    ((exp(2.163)-1)*100)*0.01 #exp to percent change & change unit increase scale to 0.01 well/sq mi
    
    ##m_income
    (-1.902e-05) - (2*3.711e-06) #lower bound
    ((exp(-2.6442e-05)-1)*100)*1000 #exp to percent change & change unit increase scale to $1000
    
    (-1.902e-05) + (2*3.711e-06) # upper bound
    ((exp(-1.1598e-05)-1)*100)*1000 #exp to percent change & change unit increase scale to $1000
    
    ##% white 
    (-2.575e-02) - (2*6.097e-03) #lower bound
    ((exp(-0.037944)-1)*100) #exp to percent change
    
    (-2.575e-02) + (2*6.097e-03) #upper bound
    ((exp(-0.013556)-1)*100) #exp to percent change
    
    ##PM2.5
    (7.071e-02) - (2*1.511e-02) #lower bound
    ((exp(0.04049)-1)*100) #exp to percent change
    
    (7.071e-02) + (2*1.511e-02) #upper bound
    ((exp(0.10093)-1)*100) #exp to percent change


#Coding for Supplementary Models 

##Model for Lag of 1-year (S2 Table)
bf_lag_model.simple <- lm(diff.logasthma.filter.2 ~ PM25_1 + lag1_year_factor, data=lag1) 
summary(bf_lag_model.simple)
  
  ##backtransform    
  (exp(0.076514)-1)*100
  
  ##Approach, calculate CI from raw values, then back-transform bounds to percent diff
  ##PM2.5
  (0.076514) - (2*0.016168) #lower bound
  ((exp(0.044178)-1)*100) #exp to percent change
  
  (0.076514) + (2*0.016168) #upper bound
  ((exp(0.10885)-1)*100) #exp to percent change

##Model for No Lag (S1 Table)
bf_no_lag_model.simple <- lm(diff.logasthma.filter.2 ~ PM25 + lag1_year_factor, data=lag1) 
summary(bf_no_lag_model.simple)

  ##backtransform    
  (exp(0.072480)-1)*100
  
  ##Approach, calculate CI from raw values, then back-transform bounds to percent diff
  (0.072480) - (2*0.015822) #lower bound
  ((exp(0.040836)-1)*100) #exp to percent change
  
  (0.072480) + (2*0.015822) #upper bound
  ((exp(0.104124)-1)*100) #exp to percent change

#--------------------------------------------------------------

###############################################################
#                     Code for Mapping                        #
###############################################################


# Load Packages
  library(rgdal)
  library(ggplot2)
  library(tidyverse)
  library(ggmap)
  library(sf)
  library(sp)

## Load Data
#insert file path unique to user system

shapes <- st_read(paste0(fldr_dir,"/Data/PaCounty2020"))
included <- read.csv(paste0(fldr_dir,"/Data/county_coding.csv"))
wells <- st_read(paste0(fldr_dir,"/Data/wells_update_take2020.shp"))   

#Creating Theme
  my_theme <- function(){theme_bw() + theme(axis.title.y = element_blank(), axis.title.x = element_text(size = 10, hjust = 0.5), axis.ticks = element_blank(), axis.text = element_blank(), legend.title = element_text(size = 10), legend.text = element_text(size = 8), plot.title = element_text(size = 15, face = "bold"), plot.subtitle = element_text(size = 10), plot.caption = element_text(size = 8, hjust = 0, face = "italic"))}
  MyTheme_transparent <- theme(panel.background = element_rect(fill = "transparent"), plot.background = element_rect(fill = "transparent", color = NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.background = element_rect(fill = "transparent"), legend.box.background = element_blank(), legend.key = element_rect(fill = "transparent", colour = NA), axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5))


#Filtering
  ##Rasmussen Rural Map
  Rasmussen_Rural <- included %>%
    filter(included == 1)
  
  ##Expansion Map
  expansion.filter <- included %>%
    filter(included == 1)


#Joining
  mapjoin4 <- full_join(included, shapes, by = "COUNTY_NAM", copy=TRUE)
  mapjoin4 <- st_as_sf(mapjoin4)
  
  mapjoin44 <- full_join(expansion.filter, shapes, by = "COUNTY_NAM", copy=TRUE)
  mapjoin44 <- st_as_sf(mapjoin44)
  
  mapjoin10 <- full_join(Rasmussen_Rural, shapes, by = "COUNTY_NAM", copy=TRUE)
  mapjoin10 <- st_as_sf(mapjoin10)


#Map Prep
  shapes <- st_as_sf(shapes)
  shapes <- st_geometry(shapes)
  plot(shapes)
  
  wells <- st_as_sf(wells)
  wells <- st_geometry(wells)
  plot(wells)


#Rural v. Urban Counties Map
  transparent <- ggplot() + geom_sf(data=mapjoin4, aes(fill=Type)) + MyTheme_transparent + theme(legend.position="bottom", legend.title = element_blank(), legend.text=element_text(face="bold")) + scale_fill_manual(values=c("#009988", "#EE7733"))
  transparent

#Map for Model 2
  expansion <- ggplot() + geom_sf(data=mapjoin44, aes(fill=Expansion)) + geom_sf(data = wells, alpha = 0.7, size = 1, color = "gray22") + MyTheme_transparent + theme(legend.position="bottom", legend.title = element_blank(), legend.text=element_text(face="bold")) + scale_fill_manual(values=c("#009E73", "#999999", "#EE7733"), labels=c("Included counties", "NA", "Urban counties"), na.value = "white") 
  expansion


#--------------------------------------------------------------

###############################################################
#                     Code for Graphs                         #
###############################################################
  
#Load packages
  library(dplyr)
  library(tidyverse)
  library(ggplot2)
  library(tidyr)
  library(ggpubr)
  library(grDevices)

  
#Loading CSV files  
  #insert file path unique to user system
  
diff.d <- read.csv(paste0(fldr_dir,"/Data/diff_streamlined_07_28_2020.csv"))
MultiTest.d <- read.csv(paste0(fldr_dir,"/Data/multi.test.csv"))
HARaverages.d <- read.csv(paste0(fldr_dir,"/Data/AsthmaHAR_Yearly_Streamlined.csv"))
ActiveWells.d <- read.csv(paste0(fldr_dir,"/Data/Asthma_HAR_Active_Wells.csv"))
averages.asthma.pm.d <- read.csv(paste0(fldr_dir,"/Data/Annual_Avg_graph_asthma_PM25.csv"))
  
#Creating Theme
  theme_set(theme_pubr() + theme(legend.position = "right"))
  
#Defining Axis Labels for Graphs
  title_PM <- expression(Avg~PM~2.5~"("~mu~g/m^3~")")
  title_Asthma <- expression(Avg~Asthma~HAR~"("~per~"10,000"~cases~")")
  title_Asthma_2 <- expression(Asthma~HAR~"("~per~"10,000"~cases~")")
  
#Streamline code for graphs 06/2021
  
  #graph of asthma by urban & rural
  well_plot_urban_rural <- ggplot(data=HARaverages.d, aes(x=Year)) + geom_line(aes(y = RuralAvgRate, color = "Rural"), size = 1.15) + geom_line(aes(y = UrbanAvgRate, color = "Urban"), size=1.15) + labs(title="Average Asthma HAR in Rural & Urban Counties",  y=title_Asthma_2, x="Year", colour="Counties") + scale_x_continuous("Year", labels = as.character(HARaverages.d$Year), breaks = HARaverages.d$Year) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + scale_color_manual(values=c("#009988", "#EE7733"))
  well_plot_urban_rural
  
  #graph of asthma and PM 2.5 by rural
  well_plot_asthma_pm_rural <- ggplot(data=averages.asthma.pm.d, aes(x=year)) + geom_line(aes(y = rural_asthma_avg, colour = "Asthma"), color = "#AA3377", size = 1.15) + geom_line(aes(y=rural_pm_avg, colour="PM 2.5"), color = "#989ca3", size = 1.15) +  labs(title="Average Asthma HAR and PM 2.5 in Rural Counties", colour="Variables") + scale_x_continuous("Year", labels = as.character(averages.asthma.pm.d$year), breaks = averages.asthma.pm.d$year) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5))  + scale_y_continuous(name=title_Asthma, sec.axis = sec_axis(~.*1, name=title_PM)) + theme(axis.title.y = element_text(color="#AA3377", size=11), axis.title.y.right = element_text(color="#989ca3", size=11)) + theme(legend.position="none")
  well_plot_asthma_pm_rural
  
  #graph of asthma and PM 2.5 by urban
  well_plot_asthma_pm_urban <- ggplot(data=averages.asthma.pm.d, aes(x=year)) + geom_line(aes(y = urban_asthma_avg, colour = "Asthma"), color = "#AA3377", size = 1.15) + geom_line(aes(y=urban_pm_avg, colour="PM 2.5"), color = "#989ca3", size = 1.15) + labs(title="Average Asthma HAR and PM 2.5 in Urban Counties", x="Year", colour="Variables") + scale_x_continuous("Year", labels = as.character(averages.asthma.pm.d$year), breaks = averages.asthma.pm.d$year) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + scale_y_continuous(name=title_Asthma, sec.axis = sec_axis(~.*1, name= title_PM )) + theme(axis.title.y = element_text(color="#AA3377", size=11), axis.title.y.right = element_text(color="#989ca3", size=11)) + theme(legend.position="none")
  well_plot_asthma_pm_urban
  
  #cumulative
  well_plot_cm_2 <- ggplot(data=ActiveWells.d, mapping = aes(x=Year, y=AW_Count_cm)) + geom_point(color="#82B446") + geom_line(color="#82B446", size=1) + labs(title="Cumulative Wells", subtitle = "in Pennsylvania", y="Active Well Count", x="Year") + theme(plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5), plot.subtitle = element_text(color = "black", size = 14, hjust = 0.5), panel.background = element_rect(fill= "white", colour = "white", size = 2, linetype = "solid")) + scale_x_continuous("Year", labels = as.character(ActiveWells.d$Year), breaks = ActiveWells.d$Year) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + scale_y_continuous("Active Well Count", breaks = c(1000,2000,3000,4000,5000, 6000, 7000))
  well_plot_cm_2 
  
  #per year
  well_plot_2 <- ggplot(data=ActiveWells.d, mapping = aes(x=Year, y=AW_Count_Sum)) + geom_point(color="#4682B4") + geom_line(color="#4682B4", size=1) + labs(title="Wells 'Spudded' per Year", subtitle = "in Pennsylvania", y="Active Well Count") + theme(plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5), plot.subtitle = element_text(color = "black", size = 14, hjust = 0.5), panel.background = element_rect(fill= "white", colour = "white", size = 2, linetype = "solid")) + scale_x_continuous("Year", labels = as.character(ActiveWells.d$Year), breaks = ActiveWells.d$Year) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + scale_y_continuous("Active Well Count", breaks = c(200, 400, 600, 800, 1000, 1200, 1400, 1600))
  well_plot_2
  
#--------------------------------------------------------------
  