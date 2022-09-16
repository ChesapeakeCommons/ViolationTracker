##### Script for adding sites hand GeoCoded by Evan #### 
#### Note, please set AWS and WR tokens via Sys.SetEnviron() ### 

library(tidyverse)
library(aws.s3)
library(httr)
library(jsonlite)

# Adding Data that Evan has sent over
EvanGeoCoded <- read.csv("Worker/EvanGeoCoded/EvanGeoCoded_9_16_2022.csv")

# Previously GeoCoded sites 
RawGeoCoded <- read_csv(get_object(object = "FullSitesGeoCoded.csv", bucket = "cm-violation-tracker"))

# Pulling in sites which cannot be geocoded
RawNoGeoCode <- read_csv(get_object(object = "FullSitesNotGeoCoded.csv", bucket = "cm-violation-tracker"))


## Had to fix a couple issues for data sent on 9_16_2022 ### 
EvanGeoCodedCleaned <- EvanGeoCoded %>%
                       filter(!is.na(Latitude))%>%
                       mutate(mde8name = as.numeric(mde8name))%>%
                       mutate(Longitude = ifelse(is.na(Longitude), -78.934, Longitude))%>%
                       mutate(Longitude = ifelse(Longitude > 0, Longitude * -1, Longitude))

#### Adding Watershed Data ####
GetWatersheds <- function(Latitude,Longitude)
{
  ## Request ## 
  Response <- GET("https://api.waterreporter.org/watersheds/intersect?", 
                  query = list(lng = Longitude, lat = Latitude, access_token = Sys.getenv("WR_API_TOKEN")))
  print(Response)
  ## Parsing ## 
  ParsedJSON <- fromJSON(content(Response,as="text"))
  
  HUC8 <- ParsedJSON$feature$huc_8$name
  
  return(as.character(HUC8))
}


## Adding watershed information from WR API
for (row in 1:nrow(EvanGeoCodedCleaned))
{
  print(paste("Getting Watershed:",row))
  EvanGeoCodedCleaned$mde8name[row] <- GetWatersheds(EvanGeoCodedCleaned$Latitude[row],EvanGeoCodedCleaned$Longitude[row])
}

##### Adding New Geocoded Sites to RawGeoCoded #### 
NewGeoCoded <- rbind(EvanGeoCodedCleaned, RawGeoCoded)


#### Removing New GeoCoded Sites from RawNoGeoCode #### 
NewNoGeoCoded <- EvanGeoCoded %>%
                 filter(is.na(Latitude))%>%
                 mutate(mde8name = as.numeric(mde8name))


### Pushing to AWS ### 
# Writing to temp 
write.csv(NewGeoCoded, file.path(tempdir(), "NewGeoCoded.csv"), row.names = FALSE)

#Putting in Bucket
put_object(
  file = file.path(tempdir(), "NewGeoCoded.csv"),
  object = "FullSitesGeoCoded.csv",
  bucket = "cm-violation-tracker"
)


# Writing to temp 
write.csv(NewNoGeoCoded, file.path(tempdir(), "NewNoGeoCoded.csv"), row.names = FALSE)

#Putting in Bucket
put_object(
  file = file.path(tempdir(), "NewNoGeoCoded.csv"),
  object = "FullSitesNotGeoCoded.csv",
  bucket = "cm-violation-tracker"
)









