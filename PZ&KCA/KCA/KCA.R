################################################################################
# Project Name:         Refresh of Retail Planning Tool
# Project Nickname:     LX Refresh
# Project goal:         Refresh existing algorithms and improve 
#                       functionality & features of Planning Tool
# Project Timeline:     November 2017 - August 2018
# First Author:         Selen Onel
# Second Author:        Soothsayer Analytics www.http://soothsayeranalytics.com/
#                       Main Author:        Pranav Shah
#                       Supervisor Authors: Sree Shravan & Akshay Deshpande

## Content Description
# This code achieves the following:
# Simulate Price/Volume change to identify key competitors given outlet

## Function Description
# Calculate distance(Haversine/Euclidean/drive time) between stations
# Filter distance within a range(rad) 
# Control min & max number of stations within a range(rad)
# The function outputs a data.frame and a CSV containing key competitors data

## Input Variables to Function
# datasim: Data to do analysis. Usually it is data_7Elements in Main.code
# Max.num: Set max number(Included/At most) of key competitors in each zone. Default 10
# Min.num: Set min number(Included/At least) of key competitors in each zone. Default 3 
# rad: Set radius of each zone. Without this constraint, simulation might be 
#      really time costing. Default 0.2 miles. Could be replaced by drive time.
# distype: The way to calculate distance. Include 7 different ways 
#          1) "distHaversine"
#          2) "distCosine"
#          3) "distGeo"
#          4) "distMeeus"
#          5) "distRhumb"
#          6) "distVincentyEllipsoid" (This is really time costing)
#          7) "distVincentySphere"
# threshold: The amount of volumn change that need to be considered as key competitors


## Output of the Function
# A data.frame contains variables with name OwnID, OwnLat, OwnLon, 
#                                           CompID, CompLat, CompLon 
# A csv file contains the output above

## This analysis is designed to run in Main.code after data clean, validation and
# clustering. Load environment which is a Main.code result.
load("environment.Rdata")

# Load reuqired packages
library(tidyverse)
library(geosphere)

## If you want to futhur speed up, run code below
# library(furrr)
# plan(multiprocess)
# change all map function to future_map

#### Build up function ####
Key_Competitor <- function(datasim, threshold = 3000, distype,
                           Max.num = 10, Min.num = 3, rad = 0.8) {
  
  #The first three columns of data_7Elements are Outlet_ID, Lat/Lon. 
  #Drive time maybe appended in future.
  datasite <- datasim[, c("OUTL_ID", "LONX_NUM", "LATY_NUM")]
  
  #### Calculate distance ####
  lat <- datasite$LATY_NUM
  lon <- datasite$LONX_NUM
  dd2 <- distm(cbind(lon, lat), cbind(lon, lat), fun = distype) 
  dd2[lower.tri(dd2)] <- 0
  
  #Change col&row names to use gather funtion below. 
  colnames(dd2) <- datasite$OUTL_ID
  rownames(dd2) <- datasite$OUTL_ID
  dd2[, 1] <- rownames(dd2)
  storage.mode(dd2) <- "numeric"
  
  #Gather function convert matrix data to tidy data.
  temp0 <- gather(dd2 %>% as.data.frame(), "CompID", "dist", 2:length(lon))
  
  #### Distance/drivetime restriction ####
  # Remove rows whose distance/drive time are not in range.
  temp <- temp0 %>% 
    dplyr::filter(dist <= rad*1609.34, dist != 0) 
  colnames(temp)[1] <- "OwnID"
  temp$OwnID <- temp$OwnID %>% as.integer()
  temp$CompID <- temp$CompID %>% as.integer()
  
  # Add Lat/Lon data back to dataset
  temp1 <- temp %>% left_join(datasite, by = c("OwnID" = "OUTL_ID")) %>% 
    rename(OwnLon = LONX_NUM, OwnLat = LATY_NUM) %>% 
    left_join(datasite, by = c("CompID" = "OUTL_ID")) %>% 
    rename(CompLon = LONX_NUM, CompLat = LATY_NUM)
  
  # Copy Inverse. We had data from A to B. This will get data from B to A.
  temp2 <- temp1 %>% 
    mutate(id1 = OwnID, id2 = CompID, OwnID = id2, CompID = id1, lon1=OwnLon,
           lat1=OwnLat, lon2=CompLon, lat2=CompLat, OwnLon=lon2, OwnLat=lat2,
           CompLon=lon1, CompLat=lat1) %>%
    select(-c(id1, id2, lon1, lon2, lat1, lat2)) 
  
  # Append two dataset together.
  distance <- bind_rows(temp1, temp2) %>% filter(OwnID != CompID)
  
  #### No. of Competitors restriction ####
  radius <- distance %>% 
    mutate(RadiusSelection = dist) %>%
    select(OwnID, CompID, dist, RadiusSelection, everything()) %>%
    group_by(OwnID, RadiusSelection) %>%
    summarize(Comps = n()) %>%
    arrange(OwnID, RadiusSelection) %>%
    group_by(OwnID) %>%
    mutate(TotalComps = cumsum(Comps),
           Flag1 = TotalComps >= Max.num, # Specification of competitor number
           Flag2 = TotalComps >= Min.num,
           Ten = ifelse(sum(Flag1)>0,TRUE,FALSE),
           Zero = ifelse(sum(Flag2)>0,TRUE,FALSE))
  
  ten <- radius %>%
    filter(Ten==TRUE, Flag1==TRUE) %>%
    mutate(Min = min(RadiusSelection,na.rm=T)) %>%
    filter(RadiusSelection == Min)  
  fewer <- radius %>%
    filter(Ten==FALSE, Zero == TRUE) %>%
    mutate(Max = max(RadiusSelection,na.rm=T)) %>%
    filter(RadiusSelection == Max)  
  
  radiusSelection <- bind_rows(ten,fewer) %>% select(OwnID, RadiusSelection)
  
  distanceFiltered <- distance %>%
    left_join(radiusSelection) %>%
    select(OwnID, CompID, dist, RadiusSelection, 
           OwnLat,OwnLon,CompLat,CompLon, everything()) %>% 
    filter(dist <= RadiusSelection)
  
  #### Do price/volumn simulation ####
  datavol <- datasim %>% select(OUTL_ID, dependentBrand, dependentVolume)
  
  price_sim <- distanceFiltered %>% 
    group_by(OwnID) %>% nest() %>% .$data %>% 
    map(left_join, datasim, by = c("CompID" = "OUTL_ID")) %>%
    map(rename, OUTL_ID = CompID) %>% 
    map(calculation_for_6_Elements, price_variables, "Price") %>% bind_rows() 
  
  price_loc <- distanceFiltered %>% mutate(ID = OwnID) %>% 
    group_by(ID) %>% nest() %>% .$data %>% bind_rows()
  
  price_vol <- bind_cols(price_sim, price_loc) %>% 
    left_join(datavol) %>% 
    mutate(volsim = PriceScore*dependentVolume) %>% 
    filter(volsim >= threshold*10)
  
  #### Combine All data into one data.frame and output it ####
  return(price_vol)
  write_csv(price_vol,"kca.csv")
}

#### test ####

time1 <- Sys.time()
KCA <- Key_Competitor(datasim = data_7Elements, distype = distHaversine)
time2 <- Sys.time()

time2 - time1