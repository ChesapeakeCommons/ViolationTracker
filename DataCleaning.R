##### INSPECTING AND CLEANING RAW DATA FOR THE VIOLATION TRACKER ### 



library(tidyverse)




### Imports ### 
GeoCoded <- read_csv("Data/GeoCode/GeoCodedFacilities.csv")
UnGeoCoded <- read_csv("Data/GeoCode/GeoCodeReview.csv") %>%
              distinct(`Site No`, .keep_all = TRUE)

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



ComplianceLocations <- Compliance %>%
                      select(Site.No,Site.Name,Street.Address,County,City..State.Zip)%>%
                      distinct(Site.No, .keep_all = TRUE)%>%
                      mutate(Type = "Compliance")

EnforcementLocations <- Enforcement %>%
                      select(Site.No,Site.Name,Street.Address,County,City..State.Zip)%>%
                      distinct(Site.No, .keep_all = TRUE)%>%
                      mutate(Type = "Enforcement")


ViolationLocations <- Violation %>%
                      select(Site.No,Site.Name,Street.Address,County,City..State.Zip)%>%
                      distinct(Site.No, .keep_all = TRUE)%>%
                      mutate(Type = "Violation")


LocationsAddresses <- rbind(ViolationLocations,ComplianceLocations,EnforcementLocations)%>%
             distinct(Site.No, .keep_all = TRUE)

write.csv(LocationsAddresses, "Data/GeoCode/LocationsAddresses_v1.csv")





## Finding list of uniques by Site No, and Location Nam
UniqueGeoCodeLatLong <- GeoCoded %>%
                          distinct(Latitude,Longitude, .keep_all = TRUE)%>%
                          mutate(LatLongPair = paste(as.character(Latitude),as.character(Longitude), sep = ""))

# UniqueGeoCodeOnlyNames <- GeoCoded %>% 
#                           distinct(Site.Name)
# UniqueGeoCodeOnlyNo <- GeoCoded %>% 
#                          distinct(Site.No)
# 
# ## Ok there are more unique site No's (8527) then unique site names (8473), meaning that some site numbers share a site name - finding which ones below ## 
UniqueGeoCodeNo_Names <- GeoCoded %>%
                         distinct(Site.No,Site.Name, .keep_all = TRUE)%>%
                           mutate(LatLongPair = paste(as.character(Latitude),as.character(Longitude), sep = ""))
# 
# Duplicated <- UniqueGeoCodeNo_Names[duplicated(UniqueGeoCodeNo_Names$Site.Name)|duplicated(UniqueGeoCodeNo_Names$Site.Name, fromLast=TRUE),]
 DuplicatedLatLong <- UniqueGeoCodeNo_Names[duplicated(UniqueGeoCodeNo_Names$LatLongPair)|duplicated(UniqueGeoCodeNo_Names$LatLongPair, fromLast=TRUE),]
# #Ok looking at this list, the Site.No does refer to different sites despite the same name. Ie  different permitIDs, different Lat Longs, etc - I think we are good to continue. 


GeoLocatedTrimmed <- GeoCoded %>%
                     select(c(Site.No,Latitude,Longitude))%>%
                     distinct(Site.No, .keep_all = TRUE)


### JOING LAT LONGS ### 
## Gabe 4.6.2021 - the GeolocatedTrimmed does not contain the full universe of locations. Sending list of locations form each file to john for easy geocoding 
ViolationGeo <- left_join(Violation, GeoLocatedTrimmed)
EnforcementGeo <- left_join(Enforcement, GeoLocatedTrimmed)
ComplianceGeo <-left_join(Compliance, GeoLocatedTrimmed)

### Counting NAs ## 
ViolationNa <- ViolationGeo %>%
               filter(is.na(Latitude))%>%
               distinct(Site.No)

EnforcementNa <- EnforcementGeo %>%
              filter(is.na(Latitude))%>%
              distinct(Site.No)

ComplianceNa <- ComplianceGeo %>%
              filter(is.na(Latitude))%>%
                distinct(Site.No)



ViolationTest <- anti_join(Violation, GeoLocatedTrimmed, by = "Site.No")%>%
                  distinct(Site.No)

                
ViolationTest2 <- anti_join(Violation, Compliance, by = "Site.No")%>%








