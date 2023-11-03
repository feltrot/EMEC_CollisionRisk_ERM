


# This script models the Encounter Risk of Marine Mammals, adapted from the Excel Model in 13.10.2023 Model - Orbital SLA Collision Risk,
# found on SharePoint in the folder Collision Risk
teststst <- 3636



# load libraries

require(svDialogs)
require(plyr)
require(WordR) # Install
library(officer)
library(dplyr)
library(tidyverse)


#set working directory (wd)
wd <- "C:/Users/felix.trotter/OneDrive - The European Marine Energy Centre Ltd/Documents/Technical_Environment/CollisionRiskModelling/"



### load data
##================================================
# Animal data
mammal_data <- read.csv(paste0(wd, "Data/FoW_MM_AnimalData.csv"))

for(an in unique(mammal_data)){
    # filter the data by species
    sp_data <- dplyr::filter(mammal_data, species == an)
    # calculate the Surface density per square meter
    surface_density_sqm <- sp_data$SurfaceDensity_no_sqkm / 1000000
    # calculate the effective radius per species
    er_m <- sp_data$ShapeFactor * sp_data$Length_m/2    # effective radius = f * L/2
    # and its standard error
    er_se <- (1/sp_data$ShapeFactor)*sp_data$Length_SE  # standard error of effective radius
    

    # animal areal density (Da) needs to be calculated as it is a key input variable of ERM and CRM
    ###============================================================

    # first calculate the watch time of the animals during surveying. This calculation depends on the type of surveying:
        #  - fixed vantage points on land
        #  - boat-based surveys
        #  - digital areal surveys

    # if watch time has not been recorded from fixed vantage points on land:
    if(sp_data$WatchPeriod_s == NA & exists(Field_width_degr) & exists(Scan_rate){
        sp_data$WatchPeriod_s <- Field_width_degr / Scan_rate     # WatchPeriod_s = Field width (degrees) / Scan rate (degrees per second)
    # if watch time has not been recorded from boat-based surveys:
    } else if(sp_data$WatchPeriod_s == NA & exists(dist_boat_m) & exists(speed_boat_m_s){
        sp_data$WatchPeriod_s <- dist_boat_m / speed_boat_m_s    # Distance observed forward of boat (m) / speed of boat (m s-1)
    # if watch time has not been recorded from digital areal survey:
    } else(sp_data$WatchPeriod_s == NA & exists(trans_leng_m) & exists(speed_aircraft_m_s)){ # Transect length captured within image (m) / aircraft speed (m s-1)
        sp_data$WatchPeriod_s <- trans_leng_m / speed_aircraft_m_s    
    }
    

    dive_freq <- 1/(sp_data$SurfaceTime_s + sp_data$DiveTime_s) # calculate dive frequency
    prop_time_vis <- 1-dive_freq + max(0, sp_data$DiveTime_s - sp_data$WatchPeriod_s) #proportion of time visible
    # another way of writing proportion of time visible (sp_data$SurfaceTime_s + min(sp_data$WatchPeriod_s, sp_data$DiveTime_s)/(sp_data$SurfaceTime_s + sp_data$DiveTime_s)) #(ts+min(tw,tu) / (ts+tu))
    ArealDensity_sqm <- 
}




device_data <- read.csv(paste0(wd, "Data/FoW_DeviceData.csv"))





# now loop through each device

### now, if necessary, correct the animal densities for the proportion of animals underwater and for watch time effects
##=================================================

#### marine mammals ####
# use the svDialogs package
correction <- dlg_message("Do densities need to be corrected for proportion of animals underwater and watch time effects?", type = "yesno")
correction_answer <- correction$res

# if density are already aeral densities
if(correction_answer=="no"){
    data <- data

}

# if densities need correction and transformed to aeral densities
if(correction_answer=="yes"){
    data$Density <- numeric(0)
    data$DiveTime_s <- numeric(0)
    data$SurfaceTime_s <- numeric(0)
    data$WatchPeriod_s <- numeric(0)

    # Overall frequency of dives is calculated as 1/(tu + ts), the reciprocal of the total time for one dive cycle. 
    data$Frequency <- 1/(data$DiveTime_s + data$SurfaceTime_s)

    #Frequency times underwater time (Ftu) is the proportion of time spent underwater. The proportion of time at the surface is thus 1-Ftu. This is then adjusted to account for watch time: proportion visible at surface = 1-F*max(0,tu-tw). Thus areal density is Thus DA=  DS/  [  1 -F * max (0, tu-tw)  ] 
    for(row in 1:nrow(data)){
        data$ArealDensity[row] <- data$Density[row]/( 1- (data$Frequency[row]*max(c(0,(data$DiveTime_s[row]-data$WatchPeriod_s[row])))))
    }# close loop
}



### now, select if using cross-sectional area or device parameters
##==================================================

type <- dlg_list(choices = c("Cross-sectional area", "Device parameters"),
    preselect = NULL,
    multiple = FALSE,
    title = "Select if using generic cross-sectional area or specific device parameters"
)

type_answer <- type$res
# Options from cross sectional area
if(type_answer=="Cross-sectional area"){
    device_data$CrossSectionalArea_sqm <- numeric(0)
    device_data$BladeLength_m <- numeric(0) # BladeLength_m = device radius
    device_data$NonOperationalTime <- numeric(0)
    device_data$MinDepth_m <- numeric(0)
    device_data$Mean_TidalCurrentSpeed_m_s <- numeric(0)
    device_data$WaterDepth_m <- numeric(0)

    #calculate cross section
    A <- device_data$CrossSectionalArea_sqm
  }

#options for device specific
if(type_answer=="Device parameters"){
    ### Device info 
    device_data <- device_data

    # calculate the CrossSectionalArea_sqm
    A <- device_data$No_Rotors * device_data$No_BladesRotor * (device_data$Max_BladeWidth_m + 2 * data$EffectiveRadius_m) * (device_data$Radius+data$EffectiveRadius_m)
}



### Calculate Q2R based on excel table here: 
##==================================================

# Note: The blade length can be considered as the radius

# calculate the maximum depth
MaxDepth_m <- device_data$MinDepth_m + (device_data$BladeLength_m*2)

# create bins based on the minimum and maximum depth
bins <- cut(seq(device_data$MinDepth_m, MaxDepth_m, 5), breaks = seq(0,50,5))
# Create a matrix based on the bins and the number of Species
q2r_data <- matrix(nrow=length(bins), ncol = length(data$Species))
# Change the column names
colnames(q2r_data) <- data$Species
# change the rownames
rownames(q2r_data) <- bins

q2r_data <- as.data.frame(q2r_data)
q2r_data[is.na(q2r_data)]<-0


dlg_message("If species-specific depth distribution is known please insert the values in the Q2R table which will appear in the next step. If not please leave as 0 and Q2R will be calculated as twice the rotor radius dividing by water depth", type="ok")

q2r_data<- edit(q2r_data)

data$q2r<-colSums(q2r_data, na.rm = T)

if(any(data$q2r==0)){
  data$q2r[which(data$q2r==0)]<- (device_data$Radius*2)/device_data$WaterDepth
}

#Calculate density at risk
data$D<-(data$ArealDensity*data$q2r)/(device_data$Radius*2)

#calculate v blade speed relative to the water, combining tangential speed and current speed
v=(device_data$TipSpeed*device_data$CurrentSpeed)/2

#ecounter rate per second following Eq 5 in Guidance noteAssessing collision risk between underwater turbines and marine wildlife from SNH
data$ERM_second<- data$D*A* (v*(1+ (data$VerticalSpeed^2/ (3*v^2))))

#encounter rate per year 
data$ERM_year<-data$ERM_second*3600*24*365*(1-device_data$NonOperationalTime)

#avoidance at 50, 95, 98, 99%
data$ERM_year_50<- data$ERM_year*(1-0.5)
data$ERM_year_90<- data$ERM_year*(1-0.90)
data$ERM_year_95<- data$ERM_year*(1-0.95)
data$ERM_year_98<- data$ERM_year*(1-0.98)
data$ERM_year_99<- data$ERM_year*(1-0.99)