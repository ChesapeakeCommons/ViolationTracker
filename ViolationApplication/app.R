##### Violation Tracker Application ##### 
##### Documentation: https://docs.google.com/document/d/1YhQAcudtysYLMNAyIhjnYEuFEiHHChvjyzvtY3DCk_4/edit 

library(shiny)
library(shinyjs)
library(leaflet)
library(tidyverse)
library(lubridate)
library(leaflet.extras)
library(rgdal)
library(jsonlite)
library(scales)


# Define UI for Application 
ui <- fluidPage(
  theme = "styler_mobile.css",
  useShinyjs(),
    
  
  div(id='background',
    HTML('<div class="showbox">
        <div class="loader"> <svg class="circular" viewBox="25 25 50 50">
          <circle class="path" cx="50" cy="50" r="20" fill="none" stroke-width="2" stroke-miterlimit="10"/>
          </svg> </div>
        </div>')    
  ),
  div(id='background2', style='background-color: unset;'),

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
        HTML('<label class="legend-header" style="width: 100%" >Totals (2016-2020) </label>'),
        uiOutput("StatsText"),
    ),
    div(id = 'stats-container-2',
        HTML('<input id="Construction" type="checkbox"/>'),
    #  checkboxInput("Construction", "Include Construction Permits"),
   #   HTML('<input id="Construction" style="width: 13px !important; height: 13px !important; max-height: 13px !important; max-width: 13px !important;" type="checkbox" data-shinyjs-resettable-id="Construction" data-shinyjs-resettable-type="Checkbox" data-shinyjs-resettable-value="false" class="shinyjs-resettable shiny-bound-input leaflet-control-layers-selector">'),
      HTML('<span>Include Construction Permits</span>')
   #  checkboxInput("Construction", "button")
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
                  actionButton("showSidebar", ""),
                  imageOutput("MarkerIcon", width= '50px', height= '50px'),
                 
                  textOutput("SiteNo"),          
                  textOutput("MarkerIconText"),
                  textOutput("InspectionType"),  
                HTML("</div>"),  
                HTML("<div>
                        <div>"),
                HTML("    </div>
                     </div>
                     "),

                tableOutput("Table"),
                     
              
              HTML("<div id='RecordNav'>"),    
              
                actionButton("Next", ""),
                actionButton("Back",""),
                textOutput("TableIndex"),
            div(id = "Search-Link",
                tags$a(href="https://mdedataviewer.mde.state.md.us/", "Click for More Info",  target="_blank"),
            ),
              HTML("</div>"),
                
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
EJWasteWater <- suppressMessages(rgdal::readOGR("www/Data/MarylandEJLayer_v6.json", verbose = TRUE))
FacilitiesReactive <- reactiveValues(df = data.frame(Facilities))
PermitsReactive <- reactiveValues(df = data.frame())
PermitPage <- reactiveValues(X = as.numeric(1))
TypeName <- c("No Issues", "Minor Issues", "Significant Issues","Repeat Non Compliance","Resolved Violation","Unresolved Violation","Enforcement Action")
TypeCode <- c("A","B","C","D","E","F","G")
MarkerType <- data.frame(TypeName,TypeCode)

### Adding symbology to the EJ Layer
for(row in 1:nrow(EJWasteWater))
{
if(EJWasteWater$P_PWDIS_D2[row] < 50)
{
  EJWasteWater$Color[row] <- "#F7FBFF"
}
  if(EJWasteWater$P_PWDIS_D2[row] > 50)
  {
    EJWasteWater$Color[row] <- "#D1BEC3"
  }
  if(EJWasteWater$P_PWDIS_D2[row] > 75)
  {
    EJWasteWater$Color[row] <- "#AB8187"
  }
  if(EJWasteWater$P_PWDIS_D2[row] > 85)
  {
    EJWasteWater$Color[row] <- "#85444A"
  }
  if(EJWasteWater$P_PWDIS_D2[row] > 95 )
  {
    EJWasteWater$Color[row] <- "#5F070E"
  }
}

### ICON FUNCTION ### 
MapIconMaker <- function(Type, Size)
{
Icon <- makeIcon(iconUrl = paste("www/Images/Markers/",Type,".png", sep = ""), iconWidth = Size*10, iconHeight = Size*10)
return(Icon)
}


### Help info Dialog ### 
InfoModal <- modalDialog(
  title = HTML("<b> Chesapeake Legal Alliance's Maryland Violation Tracker </b>"),
  HTML("<b> Quick Start Instructions and Info: </b>"),
  HTML("<br>"),
  HTML("<li>"),
  HTML("Clicking a map cluster reveals additional clusters or individual permitted water pollution sites."),
  HTML("<br>"),
  HTML("<li>"),
  HTML("Click a facility to open an info table with water pollution permit history."),
  HTML("<br>"),
  HTML("<li>"),
  HTML("Use the Legend to control whether inspections, enforcement, or violations appear on the map."),
  HTML("<br>"),
  HTML("<li>"),
  HTML("All inspection, enforcement, and compliance data is pulled from Maryland Department of Environment’s Open MDE Portal. 
     For more info, search the site number in the"),
  tags$a(href="https://mdedataviewer.mde.state.md.us/", "Open MDE portal.",  target="_blank"),
  HTML("<br>"),
  HTML("<li>"),
  HTML("Use the Basemap Control to add or remove watershed boundaries, waste water vulnerability, or basemaps."),
  HTML("<br>"),
  HTML("<li>"),
  HTML("Wastewater vulnerability is from the EPA's EJ Screen Wastewater Discharge Indicator. For more information"),
  tags$a(href="https://www.epa.gov/sites/production/files/2015-05/documents/ejscreen_technical_document_20150505.pdf#page=55", "click here.",  target="_blank"),
  HTML("<br>"),
  HTML("<br>"),
  HTML("<b> For Additional Instructions and More Information About This Tracker Tool: </b>"),
  tags$a(href="https://www.chesapeakelegal.org/", "Click Here.",  target="_blank"),
  
  easyClose = TRUE,
  footer = NULL,
)

## show info Toggles 
observeEvent(input$showInfo, {
  showModal(InfoModal)
})

### MAP ### 
output$Map <- renderLeaflet({
leaflet("Map")%>%
            setView(lng = -76.641273, lat =39.045753, zoom = 8)%>%
            addProviderTiles("CartoDB.VoyagerLabelsUnder", group = "Streets")%>%
            addProviderTiles("Esri.WorldImagery", group = "Satellite")%>%
            hideGroup("Watersheds")%>%
            addMapPane("polygons", zIndex = 210)%>%
            addPolygons(data = MarylandHucs, color = "#b3b3b3", weight = 1, group = "Watersheds", options = pathOptions(pane = "polygons"), label = paste(MarylandHucs$mde8name, "Watershed", sep = " "))%>%
            addPolygons(data = EJWasteWater, color = ~Color, weight = 1, fillOpacity = .65, opacity = .5, group = "EJ Waste Water Vulnerability Pct.", options = pathOptions(pane = "polygons"), label = paste0("Waste Water Discharge Vulnerability: ",round(EJWasteWater$P_PWDIS_D2,1),"th Pct."))%>%
            addLayersControl(
              baseGroups = c("Streets","Satellite"),
              overlayGroups = c("Inspection", "Violation" ,"Enforcement","EJ Waste Water Vulnerability Pct.","Watersheds"), 
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
                            "<div>Enforcement History</div>",
                            "<div class='legend-sub-items-container'>",
                              "<img src='./Images/Markers/G.png' />",
                            "</div>",
                       "  </div>\"  );
                       
                            
                       $( \"span:contains('Watersheds')\" ).html(  \" ",
                          "<div class='legend-item'>",
                            "<div>Watersheds</div>",
                           
                        "  </div>\"  );
                        
                       $( \"span:contains('Vulnerability')\" ).html(  \" ",
                          "<div class='legend-item'>",
                            "<div>EJ Waste Water Vulnerability Pct. </div>",
                            "<div class='legend-sub-items-container'>",
                                "<div class='color-chart' ><div>",
                            "</div>",
                        "  </div>\"  );
                        
                        
                 $( \"span:contains('EJ Layer')\" ).html(  \" ",
                    "<div class='legend-item'>",
                    "<div> EJ Waste Water Vulnerability Pct. </div>",
                    "<div class='legend-sub-items-container'>",
                    "<img src='./Images/EJLegend.png' />",
                    "</div>",
                    "  </div>\"  );
                       
                        $('#stats-container').css('z-index','999');
                    $('#stats-container-2').css('z-index','999');
                    $('.leaflet-control-search > input').attr('placeholder', 'Search Address');
                 /*   $('#stats-container').css('display','block');
                    $('#stats-container-2').css('display','block');
                    
                     $('#Construction').css({
                        'max-width':'13px !important',
                        'max-height' : '13px !important',
                        'color' : 'red'
                     });
                     $('#Construction').css('color','red')
                */
                }     
            "))%>%
            
            addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2,hideMarkerOnCollapse = TRUE))

  # options = pathOptions(pane = "polygons"),
})

output$StatsText<- renderUI({
  # Count of Sites 
  SiteCount <- Facilities %>%
    distinct(SiteNo)%>%
    tally()%>%
    as.numeric()
  
  # Count of Inspecitons 
  InspectionCount <- nrow(Permits)
  
  #NonCompliance Count 
  NonCompliance <- Permits %>%
    filter(SiteCondition == "Noncompliance")%>%
    tally()%>%
    as.numeric()
  
  #Unresolved Sig Violation
  SignificantViolation <- Facilities %>%
    filter(MarkerShape == "F")%>%
    tally()%>%
    as.numeric()
  
  #Enforcement 
  Enforcement <- Facilities %>%
    filter(MarkerShape == "G")%>%
    tally()%>%
    as.numeric()
  tagList(
    HTML("<b>&emsp;Total Inspection Reports:</b>", comma(InspectionCount),"<br>
        <b>&emsp; &emsp;Non Compliance:</b>", comma(NonCompliance),"<br>
        <b>&emsp;Total Facility Count:</b>", comma(SiteCount),"<br>
        <b>&emsp;&emsp;Significant Violation:</b>", comma(SignificantViolation),"<br>
        <b>&emsp;&emsp;Enforcement Action taken:</b>", comma(Enforcement)),
  )
})


### Map Update ### 
observe({
if(input$Construction == FALSE)
{
Facilities <- FacilitiesReactive$df %>%
              filter(Construction == 0)
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

## Observing Map click and changing the PermitReactive to represent only the selected permits
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


## Sidebar Toggles 
observeEvent(input$showSidebar, {
  shinyjs::toggle(id = "Table")
})

output$SiteNo <- renderText({
  req(input$Map_marker_click$id)
  SiteNo <- paste("Site No. ",input$Map_marker_click$id)
  return(SiteNo)
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

### Assembles the table based on facility, also orders and selects variables based on Column Select
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

#Marker Icon for showing in the render table 
output$MarkerIcon <- renderImage({
  req(input$Map_marker_click)
  
  MarkerShape <- FacilitiesReactive$df %>%
                filter(SiteNo == input$Map_marker_click$id)%>%
                pull(MarkerShape)

  MarkerLink <- paste0("www/Images/Markers/",MarkerShape,".png", sep = "")
  list(src=MarkerLink, align = "right", width = 50, height = 50)
  
},deleteFile = FALSE)


## Marker Icon Tet
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



}

# Run the application 
shinyApp(ui = ui, server = server)
