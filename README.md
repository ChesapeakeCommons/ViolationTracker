Violation Tracker README
---------------

This document provides an overview of the Violation application, its contents, and how it functions.
---------------

Quick Links
---------------

[Git](https://github.com/ChesapeakeCommons/ViolationTracker)

[Application](https://gabriel-watson.shinyapps.io/ViolationApplication/)  

[Shinyapps.io](https://www.shinyapps.io/)



Version Control and Access 
---------------
This application is open source. You can download the application from the [git repository.](https://github.com/ChesapeakeCommons/ViolationTracker) After downloading, please fork a new branch, and name it your last name, and date. e.g. Watson_07_08. Make any changes you'd like to this branch - it is yours. If there is a bug or problem with the Master branch, please perform a pull request to make changes to the core application and to be subsequently approved by the Commons team.

Application Publishing
---------------

To republish the application, use the whirlpool icon in Rstudio to publish via [Shinyapps.io](https://www.shinyapps.io/). You can see a step by step process of this [here](https://www.r-bloggers.com/2021/05/push-button-publishing-for-shiny-apps/). You’ll need your own shinyapps.io account to publish the application. 

Application Format
---------------

The application is broken into four major sections. Within app.R, there is the UI side and Server side, there is a styler.css stylesheet, and the DataCleaner.R script. Note that some basic styling does occur in the UI side of app.R when necessary. The styler.css and UI side of app.R depend on the server side of app.R. In DataCleaner.R, raw data from Maryland Department of the Environment and other sources are pulled together and prepped for the application. If you would like to make changes to the categorization of facilities, you’ll change it in DataCleaner.R.


App.R Server Side Flow
---------------

The application first loads a series of files and instantiates variables. It then displays the facilities on the Map. There is an observeEvent for construction permits which alters the list of facilities displayed in the map. Upon Map Marker click, the Permits dataframe is narrowed down to only include the selected facility. From here, the permit data and the facility information is passed to a few rendering components which comprise the different table elements. 

-----

App.R Server Side Components
---------------

Full Documentation for each component can be found in app.R server side by using Ctr + F and searching for the component by name.

#### Imports and Variable Decelerations 

* Facilities

* Permits

* ColumnSelect

* MarylandHucs

* EJWasteWater

* FacilitiesReactive

* PermitsReactive

* TypeName

* TypeCode

* MarkerType

#### Info Modal 

* InfoModal 
* observeEvent

#### Summary Totals 
* StatsText

#### Map 
* Map
* Map Update Observe
* Map Marker Click Observe

#### Table 
* showTable toggle 
* SiteNo
* InspectionType 
* TableContentMaker 
* Table
* MarkerIcon
* MarkerIconText
* Next Button Observe 
* Back Button Observe 
* TableIndex 


#### Container build
docker build -f Worker/Dockerfile -t 833394423843.dkr.ecr.us-east-1.amazonaws.com/violationtracker_worker:dev Worker/