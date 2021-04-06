##### INSPECTING AND CLEANING RAW DATA FOR THE VIOLATION TRACKER ### 



library(tidyverse)




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
  select(c(Site.No,Latitude,Longitude))


## CHECKING THE MISSINGS ##
# ## Counting NAs ##
ViolationNa <- ViolationGeo %>%
               filter(is.na(Latitude))
EnforcementNa <- EnforcementGeo %>%
              filter(is.na(Latitude))
ComplianceNa <- ComplianceGeo %>%
              filter(is.na(Latitude))
## OK LIST of NAs IS SUPER SMALL, LOOKS LIKE WE LOCATED 98%+! ### 

## Saving out here 
ViolationGeo <- left_join(Violation, GeoLocatedTrimmed)%>%
  distinct()%>%
  filter(!is.na(Latitude))
EnforcementGeo <- left_join(Enforcement, GeoLocatedTrimmed)%>%
  distinct()%>%
  filter(!is.na(Latitude))
ComplianceGeo <-left_join(Compliance, GeoLocatedTrimmed)%>%
  distinct()%>%
  filter(!is.na(Latitude))

write.csv(ViolationGeo, "Data/GeoCode/ViolationGeoCoded_v1.csv")
write.csv(EnforcementGeo, "Data/GeoCode/EnforcementGeo_v1.csv")
write.csv(ComplianceGeo, "Data/GeoCode/ComplianceGeo_v1.csv")



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





