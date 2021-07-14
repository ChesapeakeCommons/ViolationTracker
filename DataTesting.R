##### INSPECTING AND CLEANING RAW DATA FOR THE VIOLATION TRACKER ### 
library(tidyverse)
library(ggmap)
library(tmaptools)
library(tidygeocoder)
library(ggplot2)
library(mapboxapi)
library(splitstackshape)



### Imports ### 
GeoCoded <- read_csv("Data/GeoCode/LocationGeoCodeViolationTracker_0.csv")
UnGeoCoded <- read_csv("Data/GeoCode/LocationGeoCodeViolationTrackeReview_1.csv") 

Violation <- read_csv("Data/RawData/ListOfWSASignificantViolations.3_29_2021 10_59_14 AM.csv")
Enforcement <-  read_csv("Data/RawData/ListOfWSAFormalEnforcementActions.3_29_2021 10_59_19 AM.csv")
Compliance <-  read_csv("Data/RawData/ListOfWSACompliance.3_29_2021 10_58_59 AM.csv")


### Rectifying variable names ###
names(GeoCoded)<-make.names(names(GeoCoded),unique = TRUE)
GeoCoded <- GeoCoded %>%
            rename("Longitude" = x, "Latitude" = y)
names(UnGeoCoded)<-make.names(names(UnGeoCoded),unique = TRUE)
names(Violation)<-make.names(names(Violation),unique = TRUE)
names(Enforcement)<-make.names(names(Enforcement),unique = TRUE) 
Enforcement <- Enforcement %>% 
               rename("Site.No" = SITE.No)
names(Compliance)<-make.names(names(Compliance),unique = TRUE)

### Just the trimmed Locations 
GeoLocatedTrimmed <- GeoCoded %>%
  select(c(Site.No,Street.Address,Latitude,Longitude))

## Testing Geoloacting ## 
#Pulling a subset of the Geocoded 
GeoLocationTesting <- GeoCoded %>%
                      select(c(Site.No,Street.Address,Latitude,Longitude)) %>%
                      slice_sample(n = 1000)%>%
                      as_tibble()

ggplot() +
  geom_sf() +
  geom_point(data = GeoLocationTesting, aes(x = Longitude, y = Latitude), size = 1, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-80,-74.94427), ylim = c(36.984189, 40.84189), expand = FALSE)
                        
## OSM Test 
GeoLocationTestingOSM <- geo(address = GeoLocationTestingOSM$Street.Address, method = "osm", verbose = TRUE, lat = OSMlatitude, long = OSMlongitude)

ggplot() +
  geom_sf() +
  geom_point(data = GeoLocationTestingOSM, aes(x = OSMlongitude, y = OSMlatitude), size = 1, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-80,-74.94427), ylim = c(36.984189, 40.84189), expand = FALSE)
# GeoLocationCompare <- merge(GeoLocationTestingOSM,GeoLocationTesting, by.x = "address", by.y = "Street.Address")
# 
# GeoLocationCompare <- GeoLocationCompare %>%
#                       mutate(LatDif = abs(abs(OSMlatitude) - abs(Latitude))) %>%
#                        mutate(LongDif = abs(abs(OSMlongitude) - abs(Longitude)))%>%
#                       filter(LatDif < .5)%>%
#                       filter(LongDif < .5)


GeoLocationTestingMapBox <- GeoLocationTesting
GeoLocationTestingMapBox$Cordinates <- ""

##Mapbox Test
for(i in 1:nrow(GeoLocationTestingMapBox))
{
GeoLocationTestingMapBox$Cordinates[i] <- paste(mb_geocode(str_remove(GeoLocationTestingMapBox$Street.Address[i],";"), access_token = "pk.eyJ1IjoicmRhd2VzMSIsImEiOiJ0OHNqNUFFIn0.KpaFJHMqmruQ9UFeg2ATeA", output = "coordinates"), collapse = " ")
print(i)
}

GeoLocationTestingMapBox <- cSplit(GeoLocationTestingMapBox, "Cordinates", " ")

GeoLocationTestingMapBox <- GeoLocationTestingMapBox %>%
                          rename("MBLongitude" = Cordinates_1, "MBLatitude" = Cordinates_2)

ggplot() +
  geom_sf() +
  geom_point(data = GeoLocationTestingMapBox, aes(x = MBLongitude, y = MBLatitude), size = 1, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-80,-74.94427), ylim = c(36.984189, 40.84189), expand = FALSE)






#GeoLocationTestingOSM <- geocode_OSM(GeoLocationTesting$Street.Address, details = TRUE, as.data.frame = TRUE)


# for(i in 1:nrow(GeoLocationTesting))
# {
#   # Print("Working...")
#   result <- geocode(GeoLocationTesting$Street.Address[i], output = "latlona", source = "google")
#   GeoLocationTesting$Longitude[i] <- as.numeric(result[1])
#   GeoLocationTesting$Latitude[i] <- as.numeric(result[2])
#   GeoLocationTesting$geoAddress[i] <- as.character(result[3])
# }






#### DATA TESTING ##### 

# ## OK LIST of NAs IS SUPER SMALL, LOOKS LIKE WE LOCATED 98%+! ### 
# 
# ## Saving out here 
# ViolationGeo <- left_join(Violation, GeoLocatedTrimmed)%>%
#   distinct()%>%
# #  filter(!is.na(Latitude))
# EnforcementGeo <- left_join(Enforcement, GeoLocatedTrimmed)%>%
#   distinct()%>%
# #  filter(!is.na(Latitude))
# ComplianceGeo <-left_join(Compliance, GeoLocatedTrimmed)%>%
#   distinct()%>%
# #  filter(!is.na(Latitude))
# # 
# # ## CHECKING THE MISSINGS ##
# # ## Counting NAs ##
# ViolationNa <- ViolationGeo %>%
#   filter(is.na(Latitude))%>%
#   distinct()
# EnforcementNa <- EnforcementGeo %>%
#   filter(is.na(Latitude))%>%
#   distinct()
# ComplianceNa <- ComplianceGeo %>%
#   filter(is.na(Latitude))%>%
#   distinct()
# 
# #Saving out files 
# write.csv(ViolationNa, "Data/GeoCode/ViolationNoGeo_v1.csv")
# write.csv(EnforcementNa, "Data/GeoCode/EnforcementNoGeo_v1.csv")
# write.csv(ComplianceNa, "Data/GeoCode/ComplianceNoGeo_v1.csv")
# 
# write.csv(ViolationGeo, "Data/GeoCode/ViolationGeo_v1.csv")
# write.csv(EnforcementGeo, "Data/GeoCode/EnforcementGeo_v1.csv")
# write.csv(ComplianceGeo, "Data/GeoCode/ComplianceGeo_v1.csv")



### OK THIS SECTION WAS THE FIRST ATTEMPT UNTIL I FOUND OUT THAT I DIDN'T HAVE THE FULL GEOCODED LIST  ####
# # ### Selecting just the locations and saving out ###
# ComplianceLocations <- Compliance %>%
#                       select(Site.No,Site.Name,Street.Address,County,City..State.Zip)%>%
#                       distinct(Site.No, .keep_all = TRUE)%>%
#                       mutate(Type = "Compliance")
# 
# EnforcementLocations <- Enforcement %>%
#                       select(Site.No,Site.Name,Street.Address,County,City..State.Zip)%>%
#                       distinct(Site.No, .keep_all = TRUE)%>%
#                       mutate(Type = "Enforcement")
# 
# 
# ViolationLocations <- Violation %>%
#                       select(Site.No,Site.Name,Street.Address,County,City..State.Zip)%>%
#                       distinct(Site.No, .keep_all = TRUE)%>%
#                       mutate(Type = "Violation")
# 
# 
# LocationsAddresses <- rbind(ViolationLocations,ComplianceLocations,EnforcementLocations)%>%
#              distinct(Site.No, .keep_all = TRUE)
# write.csv(LocationsAddresses, "Data/GeoCode/LocationsAddresses_v1.csv")


# 
# 
# 
# ## Finding list of uniques by Site No, and Location Nam
# UniqueGeoCodeLatLong <- GeoCoded %>%
#                           distinct(Latitude,Longitude, .keep_all = TRUE)%>%
#                           mutate(LatLongPair = paste(as.character(Latitude),as.character(Longitude), sep = ""))
# 
# # UniqueGeoCodeOnlyNames <- GeoCoded %>% 
# #                           distinct(Site.Name)
# # UniqueGeoCodeOnlyNo <- GeoCoded %>% 
# #                          distinct(Site.No)
# # 
# # ## Ok there are more unique site No's (8527) then unique site names (8473), meaning that some site numbers share a site name - finding which ones below ## 
# UniqueGeoCodeNo_Names <- GeoCoded %>%
#                          distinct(Site.No,Site.Name, .keep_all = TRUE)%>%
#                            mutate(LatLongPair = paste(as.character(Latitude),as.character(Longitude), sep = ""))
# # 
# # Duplicated <- UniqueGeoCodeNo_Names[duplicated(UniqueGeoCodeNo_Names$Site.Name)|duplicated(UniqueGeoCodeNo_Names$Site.Name, fromLast=TRUE),]
#  DuplicatedLatLong <- UniqueGeoCodeNo_Names[duplicated(UniqueGeoCodeNo_Names$LatLongPair)|duplicated(UniqueGeoCodeNo_Names$LatLongPair, fromLast=TRUE),]
# # #Ok looking at this list, the Site.No does refer to different sites despite the same name. Ie  different permitIDs, different Lat Longs, etc - I think we are good to continue. 
# 
# 
# GeoLocatedTrimmed <- GeoCoded %>%
#                      select(c(Site.No,Latitude,Longitude))%>%
#                      distinct(Site.No, .keep_all = TRUE)
# 
# 

# ViolationTest <- anti_join(Violation, GeoLocatedTrimmed, by = "Site.No")%>%
#                   distinct(Site.No)
# 
#                 
# ViolationTest2 <- anti_join(Violation, Compliance, by = "Site.No")%>%





