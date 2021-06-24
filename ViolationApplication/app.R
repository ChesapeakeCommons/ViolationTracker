##### Violation Tracker Application ##### 
##### Documentation: https://docs.google.com/document/d/1YhQAcudtysYLMNAyIhjnYEuFEiHHChvjyzvtY3DCk_4/edit 

library(shiny)
library(shinyjs)
library(leaflet)
library(tidyverse)
library(lubridate)
library(leaflet.extras)
library(rgdal)


# Define UI for Application 
ui <- fluidPage(
  theme = "styler.css",
  useShinyjs(),
    

  #Start Header
  div(id = "header",
      HTML("<a href='https://www.chesapeakelegal.org/' target='blank'>"),
         div(id = "header-title"),
      HTML("</a>"),
      div(id = "title",
          HTML("Violation Tracker")
      ),
     
      div(id = "header-logo",
          actionButton("showInfo", ""),
      ),
     
      
      
      
  ),      
  #END Header
  
  

  mainPanel(
    
    leafletOutput("Map", height = 'calc(100vh - 75px)', width = '100%'),
    
    div(id = 'stats-container',
        HTML('<label class="legend-header" style="width: 100%" >Totals</label>'),
        uiOutput("StatsText"),
    ),
    
    div(id = 'stats-container-2',
      checkboxInput("Construction", "Include Construction Permits"),
      
      
      
    )
  ),
  
  tags$head(
    tags$style(
      HTML("td:first-child, td:nth-child(1){ font-weight: bold }")
    )
  ),
  
  hidden(
          div(id = "Table",
             
             sidebarPanel(width = 4,
                          
                HTML("<div class='table-container'>
                          <div class='table-header'>
                     "),          
                  imageOutput("MarkerIcon", width= '50px', height= '50px'),
                 
                  textOutput("SiteNo"),          
                  textOutput("MarkerIconText"),
                
                  actionButton("showSidebar", "Hide Table"),
                
                HTML("</div>"),  
          
                HTML("<div>
                        <div>"),
                  
                
                
                HTML("    </div>
                     </div>
                     "),
                  
                
              textOutput("InspectionType"),      
                  tableOutput("Table"),
                  textOutput("TableIndex"),
                  actionButton("Back","Back"),
                  actionButton("Next", "Next"),
                
                
                HTML("</div>"),  
              )
          )
      ),
  
  )
    
# Define server logic
server <- function(input, output, session) {
  
    
#### DATA IMPORT AND VARIABLE DECLERATION #####
Facilities <- read.csv("www/Data/Facilities_v5.csv", stringsAsFactors = FALSE)
Permits <- read.csv("www/Data/Permits_v2.csv", stringsAsFactors = FALSE)
ColumnSelect <- read_csv("www/Data/ColumnSelectSheet_v1.csv")
MarylandHucs <- suppressMessages(rgdal::readOGR("www/Data/MarylandHucs_v4.geojson", verbose = TRUE))
FacilitiesReactive <- reactiveValues(df = data.frame(Facilities))
PermitsReactive <- reactiveValues(df = data.frame())
PermitPage <- reactiveValues(X = as.numeric(1))
TypeName <- c("No Issues", "Minor Issues", "Significant Issues","Repeat Non Compliance","Resolved Violation","Unresolved Violation","Enforcement Action")
TypeCode <- c("A","B","C","D","E","F","G")
MarkerType <- data.frame(TypeName,TypeCode)


### ICON FUNCTION ### 
MapIconMaker <- function(Type, Size)
{
Icon <- makeIcon(iconUrl = paste("www/Images/Markers/",Type,".png", sep = ""), iconWidth = Size*10, iconHeight = Size*10)
return(Icon)
}


### MAP ### 
output$Map <- renderLeaflet({
leaflet("Map")%>%
            setView(lng = -76.641273, lat =39.045753, zoom = 8)%>%
            addProviderTiles("CartoDB.VoyagerLabelsUnder", group = "Streets")%>%
            addProviderTiles("Esri.WorldImagery", group = "Satellite")%>%
            hideGroup("Watersheds")%>%
            addMapPane("polygons", zIndex = 210)%>%
            addPolygons(data = MarylandHucs, color = "#b3b3b3", weight = 1, group = "Watersheds", options = pathOptions(pane = "polygons"), label = paste(MarylandHucs$mde8name, "Watershed", sep = " "))%>%
            addLayersControl(
             
              baseGroups = c("Streets","Satellite"),
  #           overlayGroups = groups, 
              overlayGroups = c("Inspection", "Violation" ,"Enforcement","Watersheds"), 
              position =c("topleft"), 
              options = layersControlOptions(collapsed = FALSE))%>%
            htmlwidgets::onRender(paste("
                function() {
                    $('.leaflet-control-layers-overlays').prepend('<label class=\"legend-header\">Legend</label>');
                    $('.leaflet-control-layers-base').prepend('<label class=\"legend-header\">Basemaps</label>');
                    
                    $( \"span:contains('Inspection')\" ).html(  \" ",
                    "<div class='legend-item'>",
                                "<div>Inspection History</div>",
                                "<div class='legend-sub-items-container'>",
                                  "<img src='./Images/Markers/A.png' /><div> No Issues</div>",
                                  "<img src='./Images/Markers/B.png' /><div> Minor Issues</div>",
                                  "<img src='./Images/Markers/C.png' /><div> Significant Issues</div>",
                                  "<img src='./Images/Markers/D.png' /><div> Repeat Non-Compliance</div>",
                                "</div>",
                                "<div>Inspection Count</div>",
                                "<div class='legend-sub-items-container'>",
                                  "<div class='size-chart' ><div>",
                                
                                "</div>",
                              "</div> \"  );
                              
                     $( \"span:contains('Violation')\" ).html(  \" ",
                          "<div class='legend-item'>",
                            "<div>Violation History</div>",
                            "<div class='legend-sub-items-container'>",
                            "<img src='./Images/Markers/E.png' /><div> Resolved</div>",
                            "<div class='legend-sub-items-container' style='margin-left: 10px; width: 100px; float:left;'><img ' src='./Images/Markers/F.png' /><div> Unresolved</div>",
                                      "</div>",
                                 
                    
                            "  </div>\"  );
                            
                       $( \"span:contains('Enforcement')\" ).html(  \" ",
                          "<div class='legend-item'>",
                            "<div>Enforcement Action</div>",
                            "<div class='legend-sub-items-container'>",
                              "<img src='./Images/Markers/G.png' />",
                            "</div>",
                       "  </div>\"  );
                       
                    $('#stats-container').css('display','block');
                    $('#stats-container-2').css('display','block');
                }     
            "))%>%
            
            addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2,hideMarkerOnCollapse = TRUE))

  # options = pathOptions(pane = "polygons"),
})


### Map Update ### 
observe({
if(input$Construction == FALSE)
{
Facilities <- FacilitiesReactive$df %>%
              filter(Construction != 0)
}
else
{
Facilities <- FacilitiesReactive$df
}

### Splitting by Marker Type ### 
Compliance <- Facilities %>%
      filter(Type == "C")

Enforcement <- Facilities %>%
      filter(Type == "E")

Violation <- Facilities %>%
  filter(Type == "V")

    
leafletProxy("Map")%>%
clearMarkers()%>%
clearMarkerClusters()%>%
addMarkers(data = Compliance, group = "Inspection", lng = ~Longitude, lat = ~Latitude, layerId = ~SiteNo, label = ~ paste("Site #:",SiteNo), icon = ~MapIconMaker(MarkerShape,MarkerSize), clusterOptions = markerClusterOptions(maxClusterRadius = 50, zoomToBoundsOnClick = TRUE, showCoverageOnHover = FALSE))%>%
addMarkers(data = Violation, group = "Violation", lng = ~Longitude, lat = ~Latitude, layerId = ~SiteNo, label = ~ paste("Site #:",SiteNo), icon = ~MapIconMaker(MarkerShape,MarkerSize), clusterOptions = markerClusterOptions(maxClusterRadius = 100, showCoverageOnHover = FALSE))%>%
addMarkers(data = Enforcement, group = "Enforcement", lng = ~Longitude, lat = ~Latitude, layerId = ~SiteNo, label = ~ paste("Site #:",SiteNo), icon = ~MapIconMaker(MarkerShape,MarkerSize), clusterOptions = markerClusterOptions(maxClusterRadius = 100, zoomToBoundsOnClick = TRUE, showCoverageOnHover = FALSE))
})

## Changing the PermitReactive to represent only the selected permits
observeEvent(input$Map_marker_click, ignoreNULL = FALSE,
             {
               if(is.null(input$Map_marker_click))
                {
                 shinyjs::hide(id = "Table")    
                 showModal(InfoModal)
                }
               else
                {
                PermitsReactive$df <- Permits %>%
                filter(SiteNo == input$Map_marker_click$id)%>%
                mutate(SiteNo = as.character(SiteNo))
                
                PermitPage$X <- 1
                
                shinyjs::show(id = "Table")    
                }
             })

InfoModal <- modalDialog(
title = HTML("<b> Chesapeake Legal Alliance's Violation Tracker </b>"),
HTML("<b> Quick Start Instructions: </b>"),
HTML("<br>"),
HTML("<li>"),
HTML("Clicking a map cluster will reveal additional clusters or individual facilities."),
HTML("<br>"),
HTML("<li>"),
HTML("Click a facility to open an information table with its permit history."),
HTML("<br>"),
HTML("<li>"),
HTML("Use the Legend to control whether inspections, enforcement, or violations appear on the map."),
HTML("<br>"),
HTML("<li>"),
HTML("To find more information, search the site number in the"),
tags$a(href="http://mdewin64.mde.state.md.us/ECollaboration/SearchPortal.aspx", "Open MDE portal."),
HTML("<br>"),
HTML("<li>"),
HTML("Use the Basemap Control to add or remove watershed boundaries, environmental justice communities, or basemaps."),
HTML("<br>"),
HTML("<br>"),
HTML("<b> For Additional Instructions and More Information About This Tracker Tool: </b>"),
tags$a(href="https://www.chesapeakelegal.org/", "Click Here."),

easyClose = TRUE,
footer = NULL,
)

## Sidebar Toggles 
observeEvent(input$showInfo, {
showModal(InfoModal)
})

## Sidebar Toggles 
observeEvent(input$showSidebar, {
  shinyjs::toggle(id = "Table")
})

## Inspection Type Text
output$InspectionType <- renderText({
  if(nrow(PermitsReactive$df) != 0)
  {
  Type <- PermitsReactive$df %>%
    arrange(desc(InspectionDate))%>%
    slice(PermitPage$X)%>%
    pull(Type)
  
  if(Type == "C")
  {
  ReportType <- "Inspection Report"
  }
  if(Type == "E")
  {
    ReportType <- "Enforcement Report"
  }
  if(Type == "V")
  {
    ReportType <- "Violation Report"
  }
  ReportType <- paste("",ReportType, "")
  }
  else
  {
  ReportType <- "Select a Location"
  }
  return(ReportType)
  
})

output$SiteNo <- renderText({
req(input$Map_marker_click$id)
SiteNo <- paste("Site No. ",input$Map_marker_click$id)
return(SiteNo)
})

## Next Button for Table 
observeEvent(input$Next, {
  if(PermitPage$X < nrow(PermitsReactive$df))
  {
    PermitPage$X <- PermitPage$X + 1
  }
})

## Back Button for Table 
observeEvent(input$Back, {
  if(PermitPage$X > 1)
  {
    PermitPage$X <- PermitPage$X - 1
  }
})

## Table Index Text 
output$TableIndex <- renderText({
  if(nrow(PermitsReactive$df) != 0)
  {
    Index <- paste(PermitPage$X, "of", nrow(PermitsReactive$df))
    return(Index)
  }    
})

TableContentMaker <- function(DF,Id)
{
  TableContent <- DF %>%
    arrange(desc(InspectionDate))%>%
    slice(Id)%>%
    mutate(InspectionDate = as.Date(InspectionDate))%>%
    mutate(ResolvedDate = as.Date(ResolvedDate))%>%
    mutate(ViolationSNCDate = as.Date(ViolationSNCDate))%>%
    mutate(EnforcementActionIssued = as.Date(EnforcementActionIssued))%>%
    mutate(CaseClosed = as.Date(CaseClosed))%>%
    mutate_if(is.Date, ~ format(.,"%B, %d, %Y"))%>%
    mutate(CityStateZip = str_replace_all(CityStateZip, ",", ", "))

    PermitType <- TableContent$Type
  
  ## Transposing to Long
  TableContent <- as.data.frame(t(TableContent))

  #Prepping to Join
  TableContent <- TableContent %>%
    mutate(OldName = rownames(.))%>%
    select(c(OldName,V1))
  
  ## Joining to the Column Select, and selecting the New Names
  Combined <- left_join(TableContent,ColumnSelect, by.x = c("Type","OldName"), by.y = c("Type","OldName"))%>%
    filter(Type == PermitType)%>%
    select(NewName,V1)%>%
    filter(!is.na(V1))
    
  #Adding ':' to the content names
  Combined$NewName <-sub("$", ":", Combined$NewName)

  return(Combined)
}

## Render Table 
# Might switch this to a different format for stylistic reasons later on 5.6.2021
output$Table <- renderTable({
   if(nrow(PermitsReactive$df) != 0)
   {
   PermitPrint <- TableContentMaker(PermitsReactive$df, PermitPage$X)
  
   ## Adding Watershed Names from the facilities file
   V1 <- FacilitiesReactive$df %>%
               filter(SiteNo == input$Map_marker_click$id)%>%
               select(mde8name)%>%
               pull()
   
   NewName <- "Watershed:"

   Watershed <- data.frame(NewName, V1)
  
   FullPermitPrint <- rbind(Watershed,PermitPrint)
              
   
   return(FullPermitPrint)
   }             
 },
 caption.placement = getOption("xtable.caption.placement", "top"), 
 include.rownames=FALSE,
 include.colnames=FALSE,
 spacing = "s" 
# width = "190px"
)

output$TableText <- renderUI({
  PermitPrint <- TableContentMaker(PermitsReactive$df, PermitPage$X)
  
  ## Adding Watershed Names from the facilities file
  V1 <- FacilitiesReactive$df %>%
    filter(SiteNo == input$Map_marker_click$id)%>%
    select(mde8name)%>%
    pull()
  
  NewName <- "Watershed:"
  
  Watershed <- data.frame(NewName, V1)
  
  FullPermitPrint <- rbind(Watershed,PermitPrint)
})


#Marker Icon for showing in the render table 
output$MarkerIcon <- renderImage({
  req(input$Map_marker_click)
  
  MarkerShape <- FacilitiesReactive$df %>%
                filter(SiteNo == input$Map_marker_click$id)%>%
                pull(MarkerShape)

  MarkerLink <- paste0("www/Images/Markers/",MarkerShape,".png", sep = "")
  list(src=MarkerLink, align = "right", width = 50, height = 50)
  
},deleteFile = FALSE)



output$MarkerIconText <- renderText({
  req(input$Map_marker_click)
  
  MarkerShape <- FacilitiesReactive$df %>%
    filter(SiteNo == input$Map_marker_click$id)%>%
    pull(MarkerShape)
  
  MarkerIconText <- MarkerType %>%
                    filter(TypeCode == MarkerShape)%>%
                    select(TypeName)%>%
                    pull()%>%
                    as.character()
  

})

output$StatsText<- renderUI({

# Count of Sites 
SiteCount <- Facilities %>%
             distinct(SiteNo)%>%
             tally()

# Count of Inspecitons 
InspectionCount <- nrow(Permits)

#NonCompliance Count 
NonCompliance <- Permits %>%
                filter(SiteCondition == "Noncompliance")%>%
                tally()

#Unresolved Sig Violation
SignificantViolation <- Facilities %>%
                        filter(MarkerShape == "F")%>%
                        tally()
                      
#Enforcement 
Enforcement <- Facilities %>%
               filter(MarkerShape == "G")%>%
               tally()
tagList(
  HTML("<b>Inspection Reports:</b>", InspectionCount),
  HTML("<br>"),
  HTML("<b>Non Compliance Inspection Reports:</b>", paste(NonCompliance)),
  HTML("<br>"),
  HTML("<b>Facility Count:</b>", paste(SiteCount)),
  HTML("<br>"),
  HTML("<b>Facilties w/ current Significant Violation:</b>", paste(SignificantViolation)),
  HTML("<br>"),
  HTML("<b>Facilities with Enforcement Action taken:</b>", paste(Enforcement)),
  
)

})

}

# Run the application 
shinyApp(ui = ui, server = server)
