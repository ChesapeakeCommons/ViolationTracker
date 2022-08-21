####### Violation Tracker Worker Application #### 
### Created by Gabe Watson 6.24.2022         ####
### Created for Chesapeake Legal ALliance    ####

library(RSocrata)
library(tidyverse)
library(tidygeocoder)
library(mapboxapi)
library(lubridate)
library(plyr)
library(httr)
library(jsonlite)
library(aws.s3)

## Steps ## 
# Import Data from Open Maryland Data Portal # 
# Import Geolocation data from S3
# Check for additional sites # 
# Geocode additional sites # 
# Export Geolocation to S3
# Score facilities # 
# Export to S3 # 
######## IMPORTS  ##### 

# Previously GeoCoded sites 
RawGeoCoded <- read_csv(get_object(object = "FullSitesGeoCoded.csv", bucket = "cm-violation-tracker"))

# Pulling in sites which cannot be geocoded
RawNoGeoCode <- read_csv(get_object(object = "FullSitesNotGeoCoded.csv", bucket = "cm-violation-tracker"))

# Raw data from MDE 
ComplianceRaw <- read.socrata("https://opendata.maryland.gov/resource/hxmu-urvx.csv")
ViolationRaw <- read.socrata("https://opendata.maryland.gov/resource/jwx7-mgcz.csv")
EnforcementRaw <- read.socrata("https://opendata.maryland.gov/resource/qbwh-5vec.csv")

####### GEOCODING #####
# Getting sites from the Data and changing some variable names 

ComplianceCleaned <- ComplianceRaw %>%
  dplyr::rename(Site.No = ai_id)

ViolationCleaned <- ViolationRaw %>%
  dplyr::rename("Site.No" = ai_id, "site_name" = ai_name)

EnforcementCleaned <- EnforcementRaw %>%
  dplyr::rename("Site.No" = ai_id, "site_name" = ai_name)

ComplianceSitesRaw <- distinct(ComplianceCleaned, Site.No, .keep_all = TRUE)%>%
  select(c(Site.No,site_name,addressinfo,city_state_zip))

ViolationSitesRaw <- distinct(ViolationCleaned, Site.No, .keep_all = TRUE)%>%
  select(c(Site.No,site_name,addressinfo,city_state_zip))

EnforcementSitesRaw <- distinct(EnforcementCleaned, Site.No, .keep_all = TRUE)%>%
  select(c(Site.No,site_name,addressinfo,city_state_zip))

## Full Site Data
SitesRaw <- distinct(rbind(ComplianceSitesRaw,ViolationSitesRaw,EnforcementSitesRaw),Site.No, .keep_all = TRUE)%>%
            mutate(Site.No = as.numeric(Site.No))

#Trimming to variables we need 
SitesGeoCoded <- RawGeoCoded %>%
  select(c(Site.No,Latitude,Longitude,site_name,addressinfo,city_state_zip, mde8name))

#Sites not in imported Geocode file 
NotGeoCoded <- anti_join(SitesRaw,SitesGeoCoded, by = "Site.No")%>%
  mutate(addressinfo = ifelse(addressinfo == "",site_name,addressinfo))%>%
  filter(str_length(addressinfo) < 80)%>%
  mutate(city_state_zip = ifelse(city_state_zip == "","MD",city_state_zip))%>%
  mutate(GeoCodeAddress = paste(addressinfo,city_state_zip))%>%
  mutate(Latitude = "")%>%
  mutate(Longitude = "")%>%
  mutate(mde8name = "")


## Removing ones which we know won't geocode
NotGeoCoded <- anti_join(NotGeoCoded,RawNoGeoCode, by = "Site.No")
              

### MapBox GeoCoding 
if(nrow(NotGeoCoded) > 0)
{
MDBoundingBox <- c(-79.487651,37.911717,-75.048939,39.723043)
MBGeoCodeResults <- NotGeoCoded
MBGeoCodeResults$LatLong <- ""
## Adding watershed information from WR API
for (row in 1:nrow(MBGeoCodeResults))
{
  print(paste("Getting Geolocation:",row))
  MBGeoCodeResults$LatLong[row] <- list(mb_geocode(MBGeoCodeResults$GeoCodeAddress[row],
                                                   access_token = Sys.getenv("MB_API_TOKEN"),
                                                   output = "coordinates", limit = 1, endpoint = "mapbox.places-permanent", search_within = MDBoundingBox))
  if(!is.null(MBGeoCodeResults$LatLong[[row]][2]))
  {
    MBGeoCodeResults$Latitude[row] <- MBGeoCodeResults$LatLong[[row]][2]
    MBGeoCodeResults$Longitude[row] <- MBGeoCodeResults$LatLong[[row]][1]
  }
}
#End Loop # 

NewGeoCoded_MB <- MBGeoCodeResults %>%
  filter(Latitude != "")%>%
  select(-c(LatLong,GeoCodeAddress))

NotGeoCoded_MB <- MBGeoCodeResults %>%
  filter(Latitude == "")%>%
  select(-c(LatLong,GeoCodeAddress))

} else {
  
  NewGeoCoded_MB <- NotGeoCoded %>%
                     select(-c(GeoCodeAddress))
  
  NotGeoCoded_MB <- NotGeoCoded %>%
                     select(-c(GeoCodeAddress))
}

#### END MB GEOCODING #####


#### Adding Watershed Data ####
GetWatersheds <- function(Latitude,Longitude)
{
  ## Request ## 
  Response <- GET("https://api.waterreporter.org/watersheds/intersect?", 
                  query = list(lng = Longitude, lat = Latitude, access_token = Sys.getenv("WR_API_TOKEN")))
  ## Parsing ## 
  ParsedJSON <- fromJSON(content(Response,as="text"))
  
  HUC8 <- ParsedJSON$feature$huc_8$name
  
  return(as.character(HUC8))
}



## Adding watershed information from WR API
for (row in 1:nrow(NewGeoCoded_MB))
{
  print(paste("Getting Watershed:",row))
  NewGeoCoded_MB$mde8name[row] <- GetWatersheds(NewGeoCoded_MB$Latitude[row],NewGeoCoded_MB$Longitude[row])
}

########################################
# New GeoCoded Data 
FullGeoCoded <- plyr::rbind.fill(SitesGeoCoded,NewGeoCoded_MB)

# All Sites with GeoCode information (if available)
FullSitesGeoCoded <- plyr::rbind.fill(SitesRaw,FullGeoCoded)%>%
  filter(!is.na(Latitude))%>%
  distinct(Site.No, .keep_all = TRUE)
  

# Writing to temp 
write.csv(FullSitesGeoCoded, file.path(tempdir(), "FullSitesGeoCoded.csv"), row.names = FALSE)

#Putting in Bucket
put_object(
  file = file.path(tempdir(), "FullSitesGeoCoded.csv"),
  object = "FullSitesGeoCoded.csv",
  bucket = "cm-violation-tracker",
  check_region = FALSE
)

## Ones we won't geocode b/c their address length is too lomg
NotGeoCodeAble <- anti_join(SitesRaw,SitesGeoCoded, by = "Site.No")%>%
                  mutate(addressinfo = ifelse(addressinfo == "",site_name,addressinfo))%>%
                  filter(str_length(addressinfo) >= 80)

# All Sites without Geocode information 
FullSitesNotGeoCoded <- plyr::rbind.fill(RawNoGeoCode,NotGeoCodeAble,NotGeoCoded_MB)%>%
  filter(!is.na(Site.No))%>%
  distinct(Site.No, .keep_all = TRUE)%>%
  mutate(mde8name = NA)

# Writing to temp 
write.csv(FullSitesNotGeoCoded, file.path(tempdir(), "FullSitesNotGeoCoded.csv"), row.names = FALSE)

#Putting in Bucket
put_object(
  file = file.path(tempdir(), "FullSitesNotGeoCoded.csv"),
  object = "FullSitesNotGeoCoded.csv",
  bucket = "cm-violation-tracker",
  check_region = FALSE
)

#######################


## Compliance 
ComplianceGeo <- left_join(ComplianceCleaned,FullSitesGeoCoded)%>%
  filter(!is.na(Latitude))%>%
  mutate(Type = "C")

## Violation 
ViolationGeo <- left_join(ViolationCleaned,FullSitesGeoCoded)%>%
  filter(!is.na(Latitude))%>%
  mutate(Type = "V")

## Enforcement 
EnforcementGeo <- left_join(EnforcementCleaned,FullSitesGeoCoded)%>%
  filter(!is.na(Latitude))%>%
  mutate(Type = "E")


## Combining into one big file for scoring 
CombinedData <- bind_rows(ComplianceGeo,EnforcementGeo,ViolationGeo) 

### Data Cleaning ###
### Renaming variables ###
## These are the correct column names !!
# SiteNo	
# SiteName
# StreetAddress	
# CityStateZip	
# County	
# InspectionType	
# InspectionDate	
# PermitNo	
# NPDESNo	
# ComplaintTrackingNo	
# InspectionReason	
# SiteStatus	
# SiteCondition	
# RecommendedActions	
# ComplianceAssist
# Latitude	
# Longitude
# Type
# EnforcementAction
# EnforcementActionNo	
# EnforcementActionIssued	
# CaseClosed	
# Media	
# Program	
# EnfIssue	
# ResolvedDate	
# ViolationSNCDate
# DocumentPage

Permits <- CombinedData %>%
  dplyr::rename(SiteNo = Site.No,
                SiteName = site_name,
                StreetAddress = addressinfo,
                CityStateZip = city_state_zip,
                County = county,
                InspectionType = inspection_type,
                InspectionDate = fir_inspection_date,
                PermitNo = permit_no,
                NPDESNo = npdes_no,
                CompliantTrackingNo = paf_no,
                InspectionReason = inspection_reason,
                SiteStatus = site_status,
                SiteCondition = site_condition,
                RecommendedActions = recommended_actions,
                ComplianceAssist = compliance_assist,
                EnforcementAction = enforcement_action,
                EnforcementActionNo = enforcement_action_no,
                EnforcementActionIssued = enforcement_action_issued,
                CaseClosed = case_closed,
                Media = media, 
                Program = program,
                EnfIssue = enf_issue,
                ResolvedDate = resolved_date,
                ViolationSNCDate = violation_date_snc_date,
                DocumentPage = documentpage)%>%
  mutate(SiteNo = as.character(SiteNo))%>%
  mutate(InspectionDate = as_date(InspectionDate))%>%
  mutate(ResolvedDate = as_date(ResolvedDate))%>%
  mutate(ViolationSNCDate = as_date(ViolationSNCDate))%>%
  mutate(EnforcementActionIssued = as_date(EnforcementActionIssued))%>%
  mutate(CaseClosed = as_date(CaseClosed))%>%
  select(-c(upload_id))


### Save out to AWS Bucket ### 
write.csv(Permits, file.path(tempdir(), "Permits.csv"), row.names = FALSE)

# Putting in Bucket
put_object(
  file = file.path(tempdir(), "Permits.csv"),
  object = "Permits.csv",
  bucket = "cm-violation-tracker",
  check_region = FALSE
)

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
# D - 14+ - Repeat Non Compliance 
# E - Resolved 
# F - Unresolved 
# G - Enforcement Action
# ### Size 
# 1 1-2 5px
# 2 3-4 10px
# 3 5-6 15px 
# 4 7+ 20px 


## Creating Construction List names
ConstructionList <- c("NPDES Construction Activity","Tidal Wetlands","Nontidal Wetlands","Sediment_Erosion")

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
        SiteSetScoringPast <- plyr::count(SiteSetPast$SiteCondition) %>%
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
    
    SiteData <- rbind(SiteData,SiteSet)
    print(nrow(SiteData))
  }
  
  # making the file unique to each site ID
  SiteData <- SiteData %>%
    distinct(SiteNo, .keep_all = TRUE)
  
  return(SiteData)
}
## Writing out to bucket ## 
Facilities <- SiteScoreMaker(Permits)

write.csv(Facilities, file.path(tempdir(), "Facilities.csv"), row.names = FALSE)

# Putting in Bucket
put_object(
  file = file.path(tempdir(), "Facilities.csv"),
  object = "Facilities.csv",
  bucket = "cm-violation-tracker",
  check_region = FALSE
)











