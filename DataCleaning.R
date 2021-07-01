##### Violation Tracker Data Cleaning ##### 
##### Documentation: https://docs.google.com/document/d/1YhQAcudtysYLMNAyIhjnYEuFEiHHChvjyzvtY3DCk_4/edit 
##### Program Use: For Updating data for the Violation Tracker once monthly ##
##### Flow: IMPORTING AND GEOLOCATING 
#####          Import new files from MDE by swapping out file handles 
#####          Check to see if new site numbers have been added 
#####          Geocode new site numbers via ESRI API
#####       DATA PROCESSING
#####          Add value type 
#####          Bind Datasets 
#####          Clean Variable Names 
#####          Marker Logic 





library(tidyverse)
library(lubridate)
library(plyr)
library(ggmap)
library(httr)
library(jsonlite)
library(sf)
library(geojsonsf)
library(rgdal)

############### ############### ###########
### IMPORTING AND GEOLOCATING ####
############### ############### ###########
LocationsOnlyGeo <- read_csv("Data/GeoCode/LocationGeoCodeViolationTracker_0.csv")
ComplianceGeo <- read_csv("Data/GeoCode/ComplianceGeo_v1.csv")
EnforcementGeo <- read_csv("Data/GeoCode/EnforcementGeo_v1.csv")
ViolationGeo <- read_csv("Data/GeoCode/ViolationGeo_v1.csv")

############### ############### ###########
############# DATA PROCESSING ########### #
############### ############### ###########

### Adding a type value ### 
ComplianceGeo <- ComplianceGeo %>%
                 mutate(Type = "C")

EnforcementGeo<- EnforcementGeo %>%
                mutate(Type = "E")

ViolationGeo<- ViolationGeo %>%
                 mutate(Type = "V")

## Counting the Number of NA's for the date catagories ## 
# ComplianceMissing <- ComplianceGeo %>%
#   summarise_all(funs(sum(is.na(.))))
# 
# EnforcementMissing <- EnforcementGeo %>%
#   summarise_all(funs(sum(is.na(.))))
# 
# ViolationMissing <-  ViolationGeo %>%
#   summarise_all(funs(sum(is.na(.))))


### Binding rows ###
CombinedData <- bind_rows(ComplianceGeo,EnforcementGeo,ViolationGeo) 
# We are looking for 37,986 rows and we got that 4.29.2021 #


### Cleaning Variable names ### 
colnames(CombinedData) <- gsub('\\.','',colnames(CombinedData))


### Data Cleaning ###
Permits <- CombinedData %>%
                 mutate(SiteNo = as.character(SiteNo))%>%
                 mutate(InspectionDate = mdy(InspectionDate))%>%
                 mutate(ResolvedDate = mdy(ResolvedDate))%>%
                 mutate(ViolationSNCDate = mdy(ViolationSNCDate))%>%
                 mutate(EnforcementActionIssued = mdy(EnforcementActionIssued))%>%
                 mutate(CaseClosed = mdy(CaseClosed))%>%
                 select(-c(X1))

## Creating Construction List 
ConstructionList <- c("NPDES Construction Activity","Tidal Wetlands","Nontidal Wetlands","Sediment_Erosion")

#### SAVING OUT PERMIT FILE ####
#Saving directly to app folder 
#write.csv(Permits, "ViolationApplication/www/Data/Permits_v2.csv", row.names = FALSE)

#CombinedDataCleaned$InspectionDate <- as.Date(CombinedDataCleaned$InspectionDate, format="%m-%d-%Y")

### Marker Logic ###
### Markers Will have a MarkerShape, and a MarkerSize parameter ### 
### Shape for A-D is based on this logic ### 
# Compliance or Unknown = 0
# Past status of Additional Investigation Required or Compliance Assistance Rendered = 1
# Current status of Additional Investigation Required or Compliance Assistance Rendered = 2 
# Past status of Noncompliance or Corrective Action = 6
# Current Noncompliance or Corrective Action = 8
### Shape 
# A - 0 - No Issues - 
# B - 1-7 - Minor Issues
# C - 8-13 - Significant Issues 
# D - 14+ - Repeat Non Compliace 
# E - Resolved 
# F - Unresolved 
# G - Enforcement Action
# ### Size 
# 1 1-2 5px
# 2 3-4 10px
# 3 5-6 15px 
# 4 7+ 20px 

#SiteConditionList <- data.frame(unique(CombinedDataTesting$SiteCondition))

CombinedDataTesting <- Permits %>%
                       slice_sample(n = 500)


SiteScoreMaker <- function(InputDF)
{
DF <- InputDF %>%
      mutate(MarkerShape = "")%>%
      mutate(MarkerSize = "")%>%
      mutate(Construction = 0)
        
      

#Declaring Empty Site Set Data for Visualizing
SiteData <- DF[0,]
 

for(i in unique(DF$SiteNo))
{
## Selecting down to only the correct site ##
SiteSet <- DF %>%
           filter(SiteNo == i)%>%
           arrange(desc(InspectionDate))


## Compliance/Inspection Scoring ##
if("C" %in% SiteSet$Type)
{
Score <- 0
PastScore <- 0
CurrentScore <- 0

## For Past Status ## 
## Only if there is more than one inspection report
if(nrow(SiteSet) > 1)
{
  
## Dropping most recent inspection report
SiteSetPast <- SiteSet %>%
           arrange(desc(InspectionDate))%>%
           slice_tail(n = nrow(.)-1)

## Calculating groups by inspection report
SiteSetScoringPast <- count(SiteSetPast$SiteCondition) %>%
                  mutate(value = 0)%>%
                  mutate(TempScore = 0)%>%
                  mutate(Score = 0)

## Generating Scoring based layed out above 
for(row in 1:nrow(SiteSetScoringPast))
{
  SiteSetScoringPast$value[row] <- ifelse(SiteSetScoringPast$x[row] == "Satisfactory/Compliance" || SiteSetScoringPast$x[row] == "UNKNOWN" || is.na(SiteSetScoringPast$x[row]),0,0)
  SiteSetScoringPast$value[row] <- ifelse(SiteSetScoringPast$x[row] == "Additional Investigation Required" || SiteSetScoringPast$x[row] == "Compliance Assistance Rendered",1,SiteSetScoringPast$value[row])
  SiteSetScoringPast$value[row] <- ifelse(SiteSetScoringPast$x[row] == "Noncompliance" || SiteSetScoringPast$x[row] == "Corrective Actions Required",6,SiteSetScoringPast$value[row])
  
  SiteSetScoringPast$TempScore[row] <- SiteSetScoringPast$value[row] * SiteSetScoringPast$freq[row]
  SiteSetScoringPast$Score <- sum(SiteSetScoringPast$TempScore)
}
PastScore <- SiteSetScoringPast$Score[1]
}


## For Current Status, only selecting the first one 
  SiteSetCurrent <- SiteSet %>%
    arrange(desc(InspectionDate))%>%
    slice_head()
  
  
## Generating Current Score 
  CurrentScore <- SiteSetCurrent %>%
                           mutate(Score = ifelse(SiteCondition == "Satisfactory/Compliance" || SiteCondition == "UNKNOWN" || is.na(SiteCondition),0,0))%>%
                           mutate(Score = ifelse(SiteCondition == "Additional Investigation Required" || SiteCondition == "Compliance Assistance Rendered",2,Score))%>%
                           mutate(Score = ifelse(SiteCondition == "Noncompliance" || SiteCondition == "Corrective Actions Required",8,Score))%>%
                           pull(Score)
  
  
## Adding past and current score for total score 
Score <- CurrentScore + PastScore


#Converting from score to Marker catagory 
SiteSet$MarkerShape <- ifelse(Score == 0,"A",NA)
SiteSet$MarkerShape <- ifelse(Score >= 1 && Score <= 7,"B",SiteSet$MarkerShape[1])
SiteSet$MarkerShape <- ifelse(Score >= 8 && Score <= 13,"C",SiteSet$MarkerShape[1])
SiteSet$MarkerShape <- ifelse(Score >= 14,"D",SiteSet$MarkerShape[1])

#Adding Marker Size
SiteSet$MarkerSize <- ifelse(nrow(SiteSet) <= 2,1,0)
SiteSet$MarkerSize <- ifelse(nrow(SiteSet) > 2,2,SiteSet$MarkerSize)
SiteSet$MarkerSize <- ifelse(nrow(SiteSet) > 7,3,SiteSet$MarkerSize)
SiteSet$MarkerSize <- ifelse(nrow(SiteSet) >= 10,4,SiteSet$MarkerSize)
}


### END INSPECTION/COMPLIANCE ###

## Enforcement ##
#Logic: If any Enforcement, then Type G
if("E" %in% SiteSet$Type)
{
  SiteSet$Type <- "E"
  SiteSet$MarkerShape <- "G"
  SiteSet$MarkerSize <- 4
}


## Violation ##
## Logic: If any Violation, then Resolved Violation (E), if no Resolved date, then Unresolved Violation(F)
if("V" %in% SiteSet$Type)
{
SiteSet$Type <- "V"
SiteSet$MarkerShape <- ifelse(is.na(SiteSet$ResolvedDate),"F","E")  
SiteSet$MarkerSize <- 4
}


## Adding Total Inspection Count
SiteSet$InspectionCount <- nrow(SiteSet)

## Adding Construction Tag
for(n in 1:nrow(data.frame(SiteSet$InspectionType)))
{
  if(SiteSet$InspectionType[n] %in% ConstructionList)
  {
    SiteSet$Construction[n] <- 1
  }
  else
  {
    SiteSet$Construction[n] <- 0
  }
}

SiteSet$Construction <- ifelse(sum(SiteSet$Construction) > 0,1,0)

# ### Testing #### 
## Comment out after 
# if(nrow(SiteSet) > 1)
# {
# print(SiteSet$SiteCondition)
# print(SiteSet$InspectionDate)
# print(SiteSet$MarkerSize)
# print(SiteSet$MarkerShape)
# print(CurrentScore)
# print(PastScore)
# print(Score)
# print("--------")
# }
# 
#print(data.frame(SiteData))

#Binding all the Site Data 

SiteData <- rbind(SiteData,SiteSet)
print(nrow(SiteData))
}



# making the file unique to each site ID
SiteData <- SiteData %>%
  distinct(SiteNo, .keep_all = TRUE)



return(SiteData)
}

Results <- SiteScoreMaker(Permits)
ResultsSF <- Results %>%
          filter(!is.na(Latitude))%>%
          filter(!is.na(Longitude))%>%
          select(SiteNo,Latitude,Longitude)


### Adding Watersheds !!! Commenting out because we got them from QGIS rather than here, but in the next update we should use this for a streamlined process ### 
## Getting MD HUCS from MD Open Data Portal)
# MDHucUrl <- "https://geodata.md.gov/imap/rest/services/Hydrology/MD_Watersheds/FeatureServer/2/query?where=1%3D1&outFields=*&outSR=4326&f=json"
# MDHucSF <- read_sf(MDHucUrl)
# crsString <- "+proj=lcc +lat_1=39.45 +lat_2=38.3 +lat_0=37.66666666666666 +lon_0=-77 +x_0=400000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
# ResultsSf <- st_read(ResultsSF,coords = c("Longitude", "Latitude"), crs = crsString)


FacilitiesOG <- read.csv("ViolationApplication/www/Data/Facilities_v5.csv", stringsAsFactors = FALSE)%>%
                select(c(SiteNo,mde8name))

FacilitiesOG$SiteNo <- as.character(FacilitiesOG$SiteNo)

ResultsFinal <- left_join(Results,FacilitiesOG)%>%
                filter(mde8name != "")

#### SAVING OUT FACILITIES FILE ####
#Saving directly to app folder 
write.csv(ResultsFinal, "ViolationApplication/www/Data/Facilities_v5.csv", row.names = FALSE)







### EXPLORING THE DATA ### 
#Looking at counts for each catagory 
MarkerShapeStats <- count(Results$MarkerShape)
MarkerSizeStats <- count(Results$MarkerSize)

PermitType <- count(Results$InspectionType)



ResultsConstructionOG <- FacilitiesOG %>%
                        filter(Construction == 1)

ResultsConstruction <- Results %>%
  filter(Construction == 1)

#Histogram 
hist(Results$InspectionCount, breaks = 200, xlim = c(0,60))
hist(ResultsNoConstruction$InspectionCount, breaks = 200, xlim = c(0,60))

##Making some maps 
SiteListMapping <- SiteList %>%
                   filter(InspectionCount > 10)
ggplot() +
  geom_sf() +
  geom_point(data = Results, aes(x = Longitude, y = Latitude), size = Results$MarkerSize, 
             shape = 1, fill = "darkred") +
  coord_sf(xlim = c(-80,-74.94427), ylim = c(36.984189, 40.84189), expand = FALSE)













