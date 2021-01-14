library(leaflet)
library(rgeos)
library(rgdal)
library(shiny)
library(RODBC)
library(shinydashboard)
library(webshot)
library(htmlwidgets)
library(ggplot2)
#library(sf)
#library(mapview)

Sys.setenv(ORACLE_HOME="/ora1/app/oracle/product/11.2.0/dbhome_1")
#webshot::install_phantomjs()

source("chooser.R") 
species <- read.csv('files/speciesTable.csv')
spp.list <- split(species$SVSPP,species$COMNAME)
strata.list <- read.csv('files/StrataList.csv')

col.bin <-function (vec1, vec.bins ) {
  n.elem=as.integer(length(vec1)) # number of elements
  n.bins = length(vec.bins)  # number of bins
  cb=rep(NA, n.elem)  # vector to store color assignments
  
  for (i in 1:n.elem) {
    test = (vec1[i] <= vec.bins)
    cb[i] = which(test==T) [1]
  }# end n.elem loop
  return(cb)
}

Strata <- readOGR("files","groundfish_strata") ## read sampling strata

big.len.calib <- read.csv('files/BigelowLenCalib.csv')
# big.len.calib <- read.csv('files/ADIOSBigelowLenCalib.csv')
# big.len.calib$STOCK <- as.character(big.len.calib$STOCK)

############adds a MINL and MAXL lengths for each species from groundfish survey data  - takes a while so just added these to the speciesTable
strata.list$AllStrata <- ifelse(nchar(strata.list$AllStrata)<5,paste0('0', strata.list$AllStrata),strata.list$AllStrata)
# for (i in species$SVSPP){
#   if(i==species$SVSPP[1]){
#     first.run <- sqlQuery(sole,paste0("select MIN(length) as minL, MAX(length) as maxL
#       from svdbs.union_fscs_svlen
#       where cruise6 in ('", paste(c(fall.cruises,spring.cruises), collapse = "','"),"') and stratum in ('", paste(strata.list$AllStrata, collapse = "','"),"') and svspp = ", i))
#   }
#   if(i!=species$SVSPP[1]){
#     temp2 <- sqlQuery(sole,paste0("select MIN(length) as minL, MAX(length) as maxL
#       from svdbs.union_fscs_svlen
#       where cruise6 in ('", paste(c(fall.cruises,spring.cruises), collapse = "','"),"') and stratum in ('", paste(strata.list$AllStrata, collapse = "','"),"') and svspp = ", i))
#     first.run <- rbind(first.run,temp2)
#   }
# 
# }
# species <- cbind(species,first.run)
# for (i in species$SVSPP){
#   if(i==species$SVSPP[1]){
#     first.run <- sqlQuery(sole,paste0("select MIN(age) as minA, MAX(age) as maxA
#       from svdbs.union_fscs_svbio
#       where cruise6 in ('", paste(c(fall.cruises,spring.cruises), collapse = "','"),"') and stratum in ('", paste(strata.list$AllStrata, collapse = "','"),"') and svspp = ", i))
#   }
#   if(i!=species$SVSPP[1]){
#     temp2 <- sqlQuery(sole,paste0("select MIN(age) as minA, MAX(age) as maxA
#       from svdbs.union_fscs_svbio

#       where cruise6 in ('", paste(c(fall.cruises,spring.cruises), collapse = "','"),"') and stratum in ('", paste(strata.list$AllStrata, collapse = "','"),"') and svspp = ", i))
#     first.run <- rbind(first.run,temp2)
#   }
#   
# }
# species <- cbind(species,first.run)
#output of the above querry
# tab.out=species[,c(1,2,3,20,21)]
# write.csv(tab.out,file="agesOut.csv")
#bsb.in = dget("user.Inputs") #pulls in bsb inputs for testing
#print(bsb.in$strata)
print(strata.list[,1])
print(names(tags))
print(tags$option)
#print(species)
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ui <-
  dashboardPage(
    dashboardHeader(title = "NEFSC Survey Data Portal"), #"Portal" is not showing up in the sidebar
    dashboardSidebar(
      #Port user between options for mapping application or SAGA clone
      #sidebarMenu(style = "position: fixed; overflow: visible;",
      sidebarMenu(
        textInput("user", label = "Username", placeholder = "Enter Oracle username..."),
        passwordInput("pass", label = "Password", placeholder = "Enter Oracle password..."),
        
        menuItem(
          "Survey Indices", 
          tabName = "indices", 
          icon = icon("fish")
        ),
        menuItem(
          "Maps", 
          tabName = "maps", 
          icon = icon("globe")
        ),
        menuItem(
          "Help", 
          tabName = "help", 
          icon = icon("baby")
        )                  
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "indices",                        #SAGA clone application user options 
          fluidRow(
            column(6,
                   fluidRow(
                     column(4,
                            #h5(strong("Specify input file:")),
                            #fileInput("sp_info", "Choose Species/Stock Info File"),
                            h5(strong("Select strata:")),
                            #uiOutput("ui.strata")
                            chooserInput("mychooser", "Available strata", "Selected frobs", #new custom widget strata selection using chooser.R
                                         strata.list[,1], c(), size = 36, multiple = TRUE)#,
                            #strata.list[,1], bsb.in$strata, size = 36, multiple = TRUE) #used for testing - autoinputs BSB strata
                            #strata.list[,1], uiOutput("ui.sp_info")$strata, size = 36, multiple = TRUE),
                            #checkboxInput("assess_strata", label = strong("Use assessment strata"), value = TRUE)
                     ),
                     column(8,
                            
                            selectInput("species", "Select species:",              #Species drop menu
                                        choices =  species$COMNAME, 
                                        selected = "BLACK SEA BASS"),
                            #uiOutput("ui.species"),
                            #selectInput("stock", "Select stock:",              #Species drop menu
                            #            choices =  species$STOCKNAME, 
                            #            selected = "BLACK SEA BASS"),
                            fluidRow(
                              column(3,
                                     radioButtons("season", "Choose season:",               #species radio buttons - switch map check boxes to these?  Probably a good idea.
                                                  choices = list("SPRING" = "SPRING", "FALL" = "FALL"), 
                                                  selected = "SPRING")),
                              #uiOutput("ui.season")),
                              column(9,   
                                     sliderInput("years", "Select range of year(s):",            #Years slider
                                                 min = 1950, 
                                                 max = 2020,
                                                 value = c(1968,2020), sep = ""))),
                            fluidRow(
                              column(6,
                                     uiOutput("ui.len")),
                              column(6,
                                     uiOutput("ui.age"))),
                            
                            #h5(strong("SHG values (pre 2009) and TOGA values (after 2009)")),
                            fluidRow(
                              column(6,h5(strong("SHG values (pre 2009)"))),
                              column(6,h5(strong("TOGA values (post 2008)")))
                            ),  
                            fluidRow(
                              column(2,
                                     textInput("S", 
                                               label = HTML("Sta. &le;"), 
                                               value = "1", 
                                               width = "70px")),
                              column(2,
                                     textInput("H", 
                                               label = HTML("Haul &le;"), 
                                               value = "3", 
                                               width = "70px")),
                              column(2,
                                     textInput("G", 
                                               label = HTML("Gear &le;"), 
                                               value = "6", 
                                               width = "70px")),
                              column(2,
                                     textInput("Type", 
                                               label = HTML("Type &le;"), 
                                               value = "1", 
                                               width = "70px")),
                              column(2,
                                     textInput("Operation", 
                                               label = HTML("Oper. &le;"), 
                                               value = "3", 
                                               width = "70px")),
                              column(2,
                                     textInput("Gear", 
                                               label = HTML("Gear &le;"), 
                                               value = "2", 
                                               width = "70px"))
                            ),
                            fluidRow(
                              column(5,
                                     selectInput("calib_type", "Bigelow calibration:",
                                                 c("none", "convert to Albatross", "convert to Bigelow"),
                                                 selected = "none"),
                                     uiOutput("ui.big.calib")),
                              #uiOutput("ui.big.calib.stock")    ),
                              column(7,
                                     selectInput("gdv_calib", "Gear/Door/Vessel calibration:",
                                                 c("none", "specify values"),
                                                 selected = "none"),
                                     uiOutput("ui.gdv.calib"))),
                            fluidRow(
                              checkboxInput("swept_area", label = strong("Use measured swept area"), value = TRUE)
                            ),
                            
                            fluidRow(
                              column(3,
                                     #Option to run current settings
                                     actionButton("runBtn","RUN", icon("cogs"), style="color: black; background-color: orange; border-color: grey")),
                              column(5,
                                     downloadButton('downloadData', 'Download .csv Data')),
                              #download data2
                              column(4,
                                     downloadButton('downloadDataR', 'Download RData'))
                            )))
            ),
            column(6,
                   plotOutput("myPlots")
            )
            
          )
          # Show a plot of the generated survey indices by strata 
          #tabPanel("N (all sizes and ages) by strata",
          #      plotOutput("myPlots")
          #)
        ),
        tabItem(
          tabName = "maps", #Define Mapping app options
          fluidRow(
            column(width = 9,
                   leafletOutput("myMap", height = "91.5vh")
            ),
            column(width = 3,
                   #download data
                   # downloadButton('downloadMap', 'Download Map (.pdf)'),
                   # br(),
                   # br(),
                   downloadButton('downloadMapHTML', 'Download Map (.html)'),
                   h5(strong('**NOTE: Map is of raw catch data and does not include calibrations [yet].')),
                   br(),
                   br(),
                   h5(strong("User Inputs")),
                   #verbatimTextOutput( outputId = "text")
                   htmlOutput("text")
                   
            )  
            
          )
        ),#end of maps tab item  
        tabItem(
          tabName = "help",
          fluidRow(
            column(
              width=9,
              #Add help text here
              h2("CAUTION: This product should not be used to generate survey indices for stock assessments!"),
              p(paste0(" This application is intended for use as an exploratory tool only. It allows analysts to experiment with "
                       ," different specifications for stock definitions, different length and age distributions, "
                       ," calibrations, or tow success criteria. The indices generated here will differ (slightly) from "
                       ," those generated in SAGA or ADIOS! and should not be used directly in stock assessments. ADIOS! "
                       ," remains the preferred source for stock assessment inputs. ")),
              h2("Survey Indices"),
              
              p(paste0("On this tab, the user selects the strata, species, seasons, years, lengths and ages for which they would like "
                       ," the application to develop indices from the NEFSC bottom trawl survey. Users can also choose to apply calibrations or different combinations "
                       ," of SHG codes. "),
                h4("Select strata"),
                p(paste0("Click or drag to select finfish strata from the window on the left. Once the desired strata are selected "
                         ," click the right arrow to add the strata to the window on the right. The strata in the window on the "
                         ," right will be used to build survey indices. Strata may also be removed by selecting "
                         ," them from the window on the right and clicking the left arrow. ")),
                h4("Select species"),
                p(paste0("Any of the species in this drop down menu are available. Click on a species and that species will show "
                         ," in the 'select species' field indicating it has been selected. ")),
                h4("Choose season"),
                p(" Indices can be built from either the fall or spring NEFSC bottom trawl survey. Click the button corresponding to the desired season"),
                h4("Select rance of years"),
                p(" Use the slider to slect a range of years over which to build survey indices. "),
                h4("Select range of length and age"),
                p(paste0(" The sliders for length and age will automatically generate bounds based on the minimum and maximum"
                         ," observed for the slected species. The range of lengths and ages can be further restricted by moving "
                         ," the sliders. Indices at age are developed by first generating an age at length matrix using a multinomial"
                         ," fit to the observed ages at length. This matrix is then used to estimate an age for each length in the "
                         ," data. After each length has been assigned an age, any ages the user has dropped using the slider are "
                         ," removed and do not contribute to the totals for the indices at age. ")),
                h4("SHG values"),
                p(paste0(" SHG values are used to assess the quality of each survey tow. "
                         ," S stands for station value. It is a code designating the type of tow (random vs. nonrandom, e.g.). An "
                         ," S value of 1 indicates a random tow. "
                         ," H stands for haul value. It designates the relative success of the haul. H <= 3 indicates a tow "
                         ," that was probably not problematic enough to be unrepresentative of the stock in the area fished. "
                         ," G stands for gear value. It designates the condition of the gear after each tow. G <= 6 indicates "
                         ," problems not serious enough to make the tow unrepresentative. ")),
                h4("Bigelow calibration"),
                p(paste0(" Select this to include species and potentially length based (if available) calibrations based on the switch "
                         ," from the RV Albatross to the RV Bigelow in 2009. ")),
                h4("Gear/Door/Vessel calibration"),
                p(paste0(" Check this box to include standard calibrations done to account for changes in vessel and/or gear over time.")),
                h4("Run"),
                p("When the inputs above are set, press the run button to develop your survey indices. " 
                  ," A plot of the indices by year will appear when the application is finished. You can change the inputs and "
                  ," hit the run button again to develop new indices based on the slected inputs."),
                h4("Download .csv Data"),
                p(paste0(" Click this button once the plot of the indices has been displayed to download the survey indices in .csv "
                         ," format. The file will include indices at length, age and by strata, as well as the inputs selected.")),
                p(paste0("Please note that the column at the far right of the indices at length is the total across the selected"
                         ,"size and age range.")),
                em(strong("*The overall indices are for all available sizes and ages.*")),
                h4("Download Rdata"),
                p(paste0(" Click this button once the plot of the indices has been displayed to download the survey indices as "
                         ," an Rdata object. The resulting file can be read using load() in R. It will load an object called "
                         ," SAGAr into memory that will contain "
                         ," indices at length, age and by strata, as well as the inputs selected. Use str(SAGAr) to see the "
                         ," data structures. ")),
                br(),
                br(),
                
                h2("Maps"),
                p("The maps tab will generate a map based on the inputs from the Survey Indices tab."),
                h4("Download html"),
                p(paste0("This button will cause an html version of the map to be downloaded to the'Downloads' directory. "
                         ," The html map can be opened in a browser and will remain interactive. A pdf of the map after "
                         ," manipulation can be saved using the browsers 'File/Export to pdf' function. ")
                ),
                br(),
                h4("Questions?"),
                p("Contact:"),
                p("Alicia Miller x2185 alicia.miller@noaa.gov or"),
                p("Dan Hennen x2398 daniel.hennen@noaa.gov")
              )
            )
          )
        ) #end of help tab
      ) #end of tabItems
    ), #end of dashboardbody
    tags$head( #This is an adjustment to the shiny notification system to allow for scrolling in overflow text
      tags$style(
        HTML(".shiny-notification {
          height: 150px;
          overflow-y: scroll;
          }
          "
        )
      )
    )
  ) #end of dashboard page
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

server = function(input, output, session){
  source("helper.R")  #moved the stratification calculation function out the server function for ease of reading the code
  #it is now in helper.R
 
  output$ui.len <- renderUI({  #generates dynamic UI for length widget
    if (is.null(input$species))
      return()
    #Changes to speciesTable can cause problems - this makes it dynamicalong with the changes to the switch function below
    MyRow=which(species$COMNAME==input$species)

    switch(input$species,
            sliderInput("len1", "Select range of length(s):"
                , min = species$MINL[MyRow], max = species$MAXL[MyRow]
                , value = c(species$MINL[MyRow],species$MAXL[MyRow]),round=T, step=1 , sep = "")

    )
    
  })
  output$ui.age <- renderUI({  #generates dynamic UI for age widget
    if (is.null(input$species))
      return()
    
    #Changes to speciesTable can cause problems - this makes it dynamicalong with the changes to the switch function below
    MyRow=which(species$COMNAME==input$species)
    
    switch(input$species,
           sliderInput("age1", "Select range of age(s):"
                , min = species$MINA[MyRow], max = species$MAXA[MyRow]
                , value = c(species$MINA[MyRow],species$MAXA[MyRow]),round=T, step=1 , sep = "")
    )
    
  })
  output$ui.big.calib <- renderUI({  #generates dynamic UI for Bigelow calibration widget
    if (is.null(input$calib_type))
      return()
    
    # Depending on input$calib_type, we'll generate a different
    # UI component and send it to the client.
    print(names(input))
    print(input$species)
    print(input$sp_info)
    print(input$calib_type)
    print(input$user)
    print(input$pass)
    #stop()
    if (species$BIGELOWCALTYPE[species$COMNAME==input$species] == 'CONSTANT'){
      #if (species$BIGELOWCALTYPE[species$COMNAME==input$species] == 'CONSTANT'){
      switch(input$calib_type,
             "convert to Albatross" = selectInput("calib_meth", "Select calibration method:",
                                                  choices = c("constant" = "alb.const.rho")
             ),
             "convert to Bigelow" = selectInput("calib_meth", "Select calibration method:",
                                                choices = c("constant" = "big.const.rho")
             )
      )
    }
    if (species$BIGELOWCALTYPE[species$COMNAME==input$species] == 'LENGTH'){
      switch(input$calib_type,
             "convert to Albatross" = selectInput("calib_meth", "Select calibration method:",
                                                  choices = c("constant" = "alb.const.rho",
                                                              "length" = "alb.len.rho"),
                                                  selected = "alb.const.rho"
             ),
             "convert to Bigelow" = selectInput("calib_meth", "Select calibration method:",
                                                choices = c("constant" = "big.const.rho",
                                                            "length" = "big.len.rho"),
                                                selected = "big.const.rho"
             )
      )
    }
    
  })
  
  output$ui.gdv.calib <- renderUI({  #generates dynamic UI for Bigelow calibration widget
    if (is.null(input$gdv_calib))
      return()
    
    # Depending on input$calib_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$gdv_calib,
           "specify values" = fluidRow(
             column(4,
                    textInput("gear.num", 
                              label = "Gear num.", 
                              value = "1", 
                              width = "50px"),
                    textInput("gear.wt", 
                              label = "wt.", 
                              value = "1", 
                              width = "50px")),
             column(4,
                    textInput("door.num", 
                              label = "Door num." , 
                              value = "1", 
                              width = "50px"),
                    textInput("door.wt", 
                              label = "wt." , 
                              value = "1", 
                              width = "50px")),
             column(4,
                    textInput("vessel.num", 
                              label = "Vessel num.", 
                              value = "1", 
                              width = "50px"),
                    textInput("vessel.wt", 
                              label = "wt.", 
                              value = "1", 
                              width = "50px"))
           ),
           "convert to Bigelow" = selectInput("calib_meth", "Select calibration method:",
                                              choices = c("constant" = "big.const.rho",
                                                          "length" = "big.len.rho"),
                                              selected = "big.const.rho"
           )
    )
  })
  
  output$selection <- renderPrint({
    if(is.null(get_data())){return ()}
    input$mychooser$right
  })
  # print list of input events
  output$text <- renderPrint({
    #reactiveValuesToList(input[which(substr(names(input),1,4)=="map_")])
    reactiveValuesToList(input)[c("species","years","season","mychooser")] 
    #,input$mychooser$right,input$years,input$season,input$S,input$H,input$G))
  })
  
  
  #This makes a variable that is assigned inside the observe event environment, but visible outside it (use <<- to assign in
  # the observe event function).   
  All.out<-list()
  makeReactiveBinding("All.out")
  User.Inputs=list()  #don't need to make reactive (I think) because this is made of reactive values 
  
  
  observeEvent(input$runBtn,{ #if run button is pushed:
    
    print("Running")
    
    ######Pull cruise codes and add user/pwd
    #sole <- odbcConnect(dsn="sole", uid=input$user, pwd=input$pass, believeNRows=FALSE)
    #sole <- try(odbcConnect(dsn="sole", uid=input$user, pwd=input$pass, believeNRows=FALSE))
    
    #Missing sole user/password notification
    if(input$user=='') {
      showNotification("PLEASE ENTER USERNAME!!"
                       ,id="missingUSER",duration=NULL,type="error")
    } else removeNotification(id="missingUSER")
    req(input$user)
    if(input$pass=='') {
      showNotification("PLEASE ENTER PASSWORD!!"
                       ,id="missingPASS",duration=NULL,type="error")
    } else removeNotification(id="missingPASS")
    req(input$pass)
    
    #CHECK sole username/pwd
    sole <- try(odbcConnect(dsn="sole", uid=input$user, pwd=input$pass, believeNRows=FALSE))
    if(sole==-1){
      showNotification("INCORRECT USERNAME/PASSWORD!!!"
                       ,id="incorrectOracle",duration=NULL,type="error")
    } else removeNotification(id="incorrectOracle")
    req(sole != -1)#,cancelOutput = TRUE)
    
    get.survey.data.fn <- function(oc = sole, purpose.code = 10){
      q.surveys <- paste0("select cruise6,purpose_code,season,year, startdate, enddate from mstr_cruise where purpose_code=", purpose.code, 
                          " and cruise6 is not null order by year,cruise6,season")
      survey.view <- sqlQuery(oc,q.surveys)
      return(survey.view)
    }
    
    survey.cruises <- get.survey.data.fn()
    fall.cruises <- unique(survey.cruises$CRUISE6[survey.cruises$SEASON == 'FALL'])
    spring.cruises <- unique(survey.cruises$CRUISE6[survey.cruises$SEASON == 'SPRING'])
    
    #Missing Strata notification
    if(length(input$mychooser$right)==0) {
      showNotification("PLEASE CHOOSE AT LEAST ONE STRATUM!!"
                       ,id="strataChosen",duration=NULL,type="error")
    } else removeNotification(id="strataChosen") 
    req(input$mychooser$right)
    
    
    yrs=seq(min(input$years),max(input$years))
    #grab cruise6 from the rows with matching season and year
    #print("yrs")
    #print(yrs)
    #you get multiple values before 1982, maybe because of multiple vessels?
    cruise6 <- unique(survey.cruises$CRUISE6[survey.cruises$SEASON == input$season & 
                                               survey.cruises$YEAR %in% yrs])
    spp <- species$SVSPP[species$COMNAME == input$species] #species name as well
    
    strata.in <- input$mychooser$right
    len.range <- c(input$len1[1]:input$len1[2])
    do.age=T
    #print(input$age1)
    if(!is.null(input$age1[1]) & !is.null(input$age1[2])) {
      age.range <- c(input$age1[1]:input$age1[2])
    } else {
      do.age=F
      age.range <- NULL
    }
    do.Albatross <- input$calib_type=="convert to Albatross"
    do.Bigelow <- input$calib_type=="convert to Bigelow"
    if(is.null(input$calib_meth)){
      do.BigLen <- FALSE
      do.AlbLen <- FALSE
    }
    else{
      do.BigLen <- input$calib_meth=="big.len.rho"
      do.AlbLen <- input$calib_meth=="alb.len.rho"
    }
    
    if(input$gdv_calib=="specify values"){
      gcf.n<- input$gear.num
      dcf.n<- input$door.num
      vcf.n<- input$vessel.num
      gcf.w<- input$gear.wt
      dcf.w<- input$door.wt
      vcf.w<- input$vessel.wt
    } else {
      gcf.n<- 1
      dcf.n<- 1
      vcf.n<- 1
      gcf.w<- 1
      dcf.w<- 1
      vcf.w<- 1
      
    }
    
    #Check user inputs
    S<-input$S
    H<-input$H
    G<-input$G
    #TOGA place holder - needs the UI boxes and input changes! 
    Acquisition<-"X"
    Type<-input$Type
    Operation<-input$Operation
    Gear<-input$Gear
    # Acquisition<-input$Acquisition
    #make sure we have values for all these codes (users might delete them potentially)
    if(Type=="" | Type<8) Type=1
    if(Operation=="" | Operation>4)  Operation<-3
    if(Gear=="" | !Gear%in%c(1,2,4)) Gear<-2 #Gear codes only can be 1,2 or 4
    if(S=="" | S>9) S<-1
    if(H=="" | H>7) H<-3
    if(G=="" | G>9) G<-6
    #swept area
    swept_area <- input$swept_area
    
    
    #Expand to cover unsampled strata? For now this is automatic, but could be built into an reactive input
    Expansion=T
    
    #Check user inputs and make a table of them for later 
    #Check length range given user inputs (there is no reason to generate rows of zeros!):
    q.len <- paste("select MIN(length) as minL, MAX(length) as maxL  from svdbs.union_fscs_svlen " ,
                   " where cruise6 in ('", paste(cruise6, collapse="','"), "')"
                   , " and STRATUM IN('", paste(ifelse(nchar(strata.in)<5,paste0('0', strata.in),strata.in), collapse = "','")
                   , "')"," and svspp = ", spp, ";", sep = '')
    len.view <- as.data.frame(sqlQuery(sole,q.len))
    print(head(len.view))
    print(dim(len.view))
    #adjust length range and report in console
    #if(min(len.range)<len.view$MINL | max(len.range)>len.view$MAXL) print("Adjusting user requested lengths to range of observed values")
    #if(min(len.range)<len.view$MINL) len.range <- len.range[which(len.range>=len.view$MINL)]
    #if(max(len.range)>len.view$MAXL) len.range <- len.range[which(len.range<=len.view$MAXL)]
    #if(input$minLength<len.view$MINL | input$maxLength>len.view$MAXL) print(c("New range: ",len.range))
    #print(survey.cruises$CRUISE6[survey.cruises$SEASON == input$season ])
    #print(survey.cruises$CRUISE6[survey.cruises$YEAR %in% seq(min(input$years),max(input$years))])
    userInputs<<-list("species"=input$species,"strata"=strata.in,"years"=range(input$years)
                      ,"season"=input$season,"len.range"=range(len.range),"age.range"=range(age.range))
    
    #dput(userInputs,"user.Inputs") #for error trapping $$$$$$$$$$$$$$ REMOVE BEFORE POSTING $$$$$$$$$$$$$$$$$$$$$$
    
    
    showNotification(" Running... ",duration=NULL,id="running",type="message")
    
    
    if(length(cruise6)>0){
      #Destroy the saved memory objects that are outputs
      Ind.out=c();IAL.out=c();VIAL.out=c();IAA.out=c();maxL=max(len.range);minL=min(len.range);unUsedStrata=strata.in;
      UnSampledStrata=list();Expand=c();CatchData=c();
      for(i in 1:length(cruise6)) {
        
        x.out<- get.survey.stratum.estimates.2.fn(spp=spp,
                                                  survey = cruise6[i], 
                                                  oc = sole, 
                                                  strata = strata.in,
                                                  lengths = len.range,
                                                  age = age.range,
                                                  do.length = TRUE, 
                                                  do.age = do.age,
                                                  gcf.n = gcf.n, 
                                                  dcf.n = dcf.n, 
                                                  vcf.n = vcf.n, 
                                                  gcf.w = gcf.w, 
                                                  dcf.w = dcf.w, 
                                                  vcf.w = vcf.w, 
                                                  do.Albatross = do.Albatross,
                                                  do.Bigelow = do.Bigelow,
                                                  tow_swept_area = 0.01,
                                                  species=species, 
                                                  fall.cruises=fall.cruises,
                                                  spring.cruises=spring.cruises,
                                                  do.BigLen=do.BigLen,
                                                  do.AlbLen=do.AlbLen,
                                                  big.len.calib=big.len.calib,
                                                  S=S,H=H,G=G,                                              
                                                  Type=Type,
                                                  Operation=Operation,
                                                  Gear=Gear,
                                                  Acquisition=Acquisition,
                                                  swept_area=swept_area
        )
        # #Progress bar
        withProgress(message=paste0('Calculating indices for ',yrs[i]),{
          for (j in 1:10) {
            incProgress(1/10)
            
          }
        })
        
        if(nrow(x.out$catchdata[[1]])>0) CatchData<-(rbind(CatchData,x.out$catchdata[[1]])) #keep the catch data for later output
        
        #Take the important parts from x.out to generate an index over time.
        Yeari=as.integer(substr(paste(cruise6[i]),1,4))
        Tows=sum(x.out$out[,"m"]) #number of tows in the year in question
        
        #Generate products for later download and plotting:
        #grab the Num,Wt and generate stratified means 
        goodRows=(x.out$out[,"m"]>0)
        #in case there is only one strata...
        if(length(goodRows)==1) {SMns=(x.out$out[goodRows,c(4:5)]*
                                         x.out$out[goodRows,"M"])/sum(x.out$out[goodRows,"M"])
        } else SMns=colSums(x.out$out[goodRows,c(4:5)]*
                              x.out$out[goodRows,"M"],na.rm=T)/sum(x.out$out[goodRows,"M"])
        #Now get the variances
        goodRows=(x.out$out[,"m"]>1)
        if(length(goodRows)==1) {
          Svars=(x.out$out[goodRows,c(6:7)]*
                   (x.out$out[goodRows,"M"]^2))/sum(x.out$out[goodRows,"M"])^2
        } else Svars=colSums(x.out$out[goodRows,c(6:7)]*
                               (x.out$out[goodRows,"M"]^2),na.rm=T)/sum(x.out$out[goodRows,"M"])^2
        Ind.out<-rbind(Ind.out,c(Yeari,Tows
                                 ,SMns,Svars)) 
        #The indices at length require similar manipulation
        #IAL.out<-rbind(IAL.out,c(Yeari,Tows,colSums(x.out$Nal.hat.stratum/x.out$out[,"M"])
        #    ,"Total"=sum(colSums(x.out$Nal.hat.stratum/x.out$out[,"M"]))))  #Add a "Total" which is the index over the sizes of interest
        goodRows=(x.out$out[,"m"]>0)
        IAL.out<-rbind(IAL.out,c(Yeari,Tows,colSums(x.out$Nal.hat.stratum/sum(x.out$out[,"M"]))
                                 ,"Total"=sum(colSums(x.out$Nal.hat.stratum/sum(x.out$out[,"M"])))))  #Add a "Total" which is the index over the sizes of interest
        
        #divide by the stratum area to get unexpanded numbers at length
        VIAL.out<-rbind(VIAL.out,c(Yeari,Tows,colSums(x.out$V.Nal.stratum/sum(x.out$out[,"M"])^2)
                                   ,"Total"=sum(colSums(x.out$V.Nal.stratum/sum(x.out$out[,"M"])^2)))) #remove stratum area expansion
        
        
        print(c(Yeari,Tows))
        # print(sum(x.out$out[,"M"]))
        # print((x.out$Naa.hat.stratum))
        # print(colSums(x.out$Naa.hat.stratum[,2:ncol(x.out$Naa.hat.stratum)]/sum(x.out$out[,"M"])))
        # print(sum(colSums(x.out$Naa.hat.stratum[,2:ncol(x.out$Naa.hat.stratum)]/sum(x.out$out[,"M"]))))
        
        
        if(do.age) { 
          IAA.out<-rbind(IAA.out,c(Yeari,Tows,colSums(x.out$Naa.hat.stratum[,2:ncol(x.out$Naa.hat.stratum)]/sum(x.out$out[,"M"]))
                                   ,"Total"=sum(colSums(x.out$Naa.hat.stratum[,2:ncol(x.out$Naa.hat.stratum)]/sum(x.out$out[,"M"])))))
          #print(IAA.out)
        } 
        #Do we need a separate variance calc for the number at age?
        
        #Warnings next - first make sure the warnings are consistent through time
        #Get the min and max observed size for all years
        minL=min(minL,min(x.out$warnings$LengthRange),na.rm=T)
        maxL=max(maxL,max(x.out$warnings$LengthRange),na.rm=T)
        #track if any user selected strata have no observed catch for this species in any of the selected years
        unUsedStrata=unUsedStrata[unUsedStrata%in%x.out$warnings$UnusedStrata]
        #Also track unsampled strata in each year
        if(!is.null(x.out$warnings$UnsampledStrata)) {
          UnSampledStrata=append(UnSampledStrata,c(Yeari,x.out$warnings$UnsampledStrata))
        }
        #Keep the expansion factor for unsampled strata on hand for each year
        Expand=c(Expand,x.out$expand)
      }
      
      #If the user wishes to expand over unsampled strata
      if(Expansion){
        expnd=Expand
      } else expnd=rep(1.,nrow(Expand))
      
      Ind.out=as.data.frame(Ind.out)
      names(Ind.out)=c("Year","Tows","Num","Wt","VarNum","VarWt")
      
      IAL.out=as.data.frame(IAL.out)
      IAL.out[,ncol(IAL.out)]=IAL.out[,ncol(IAL.out)]*expnd #Expand the total to cover unsampled strata (if desired)
      names(IAL.out)=c('Year','nTows',paste(len.range,"cm",sep=""),'Total')
      
      VIAL.out=as.data.frame(VIAL.out)
      VIAL.out[,ncol(VIAL.out)]=VIAL.out[,ncol(VIAL.out)]*expnd^2 #Expand the total to cover unsampled strata (if desired)
      names(VIAL.out)=c('Year','nTows',paste(len.range,"cm",sep=""),'Total')
      
      IAA.out=as.data.frame(IAA.out)
      if(do.age) {
        IAA.out[,ncol(IAA.out)]=IAA.out[,ncol(IAA.out)]*expnd #Expand the total to cover unsampled strata (if desired)
        names(IAA.out)=c('Year','nTows',paste0("Age",age.range),'Total')
      } else IAA.out="No ages"
      
      CatchData=as.data.frame(CatchData)
      #print(CatchData)
      names(CatchData)=c("CRUISE6","STRATUM","TOW","STATION","SHG","SVVESSEL","SVGEAR","EST_YEAR","EST_MONTH"                
                        ,"EST_DAY","TIME","TOWDUR","DOPDISTB","DOPDISTW","AVGDEPTH","STATYPE","HAUL","GEARCOND"                 
                        ,"TYPE_CODE","OPERATION_CODE","GEAR_CODE","ACQUISITION_CODE","AREA","BOTTEMP","BEGLAT"
                        ,"BEGLON","STRATUM_AREA","STRGRP_DESC","STRATUM_NAME","AREA_SWEPT_WINGS_MEAN_KM2"
                        ,"AREA_SWEPT_WINGS_MEAN_NM2" ,"SWEPT_AREA_RATIO","SVSPP","CATCHSEX","EXPCATCHWT","EXPCATCHNUM")
      
      #try saving the data in a reactiveValues for later download 
      All.out<<-list( #Note that the assignment is done with " <<- " !!
        "Index"=Ind.out
        ,"NatLength"=IAL.out
        ,"VarNatLength"=VIAL.out
        ,"NatAge"=IAA.out
        #Add tow data to the list, including station, catch and swept area estimates
        #,"CatchData"=CatchData #unmodified from helper.r function for now
        #Add the individual lengths as well?
        
        #,"Species"=userInputs$species
        #,"Strata"=userInputs$strata
        # ,"Years"=range(userInputs$years)
        # ,"Season"=userInputs$season
        # ,"Lengths"=range(userInputs$len.range)
        # ,"Ages"=range(userInputs$age.range)
      )
      
      #plot the indices for something to look at after a successful run
      if(!is.na(x.out[[2]][1,1])) { #Check to make sure x.out was loaded before attempting to plot
        output$myPlots <- renderPlot({
          
          #plot1 <- ggplot(as.data.frame(x.out$out), aes(x=stratum, y= EXPCATCHNUM)) +
          #    geom_bar(stat="identity") +
          # theme_bw()
          if(nrow(Ind.out)>1){ #make sure there is more than one year to plot
            #calculate a confidence interval to add to plot - just using the normal approximation here
            ci=data.frame(Ind.out,"lci"=Ind.out$Wt-1.96*(sqrt(Ind.out$VarWt))  #/Ind.out$Tows
                          ,"uci"=Ind.out$Wt+1.96*(sqrt(Ind.out$VarWt) ))  #/Ind.out$Tows
            
            #alternative is the Buckland (1993) method (these are waaay bigger)
            #logvar=log(1+Ind.out$VarWt/(Ind.out$Wt)^2)
            #ci=data.frame(Ind.out,"lci"=Ind.out$Wt*logvar,"uci"=Ind.out$Wt/logvar )
            
            #print(ci)
            
            p1<-ggplot2::ggplot(Ind.out, aes(x=Year, y=Wt)) +
              ggplot2::geom_line() +
              ggplot2::geom_ribbon(data=ci,aes(ymin=lci,ymax=uci),alpha=0.3) +
              ggplot2::theme_bw() +
              ggplot2::ggtitle("Aggregate Indices (*Does not use length critera)")
              
            
            ciNum=data.frame(Ind.out,"lci"=Ind.out$Num-1.96*(sqrt(Ind.out$VarNum))  #/Ind.out$Tows
                             ,"uci"=Ind.out$Num+1.96*(sqrt(Ind.out$VarNum) ))  #/Ind.out$Tows
            print(ciNum)
            p2<-ggplot2::ggplot(Ind.out, aes(x=Year, y=Num)) +
              ggplot2::geom_line() +
              ggplot2::geom_ribbon(data=ciNum,aes(ymin=lci,ymax=uci),alpha=0.3) +
              ggplot2::theme_bw()             
            gridExtra::grid.arrange(p1,p2,ncol=1)
            
            #print(plot1)
          }
        })
      } else print("Invalid Stratum Selection: no observations of selected species in strata")
    } else print("Invalid Stratum Selection: no observations of selected species in strata")
    
    removeNotification(id="running")
    
    #show warnings in notification form and remove existing old notifications
    # if(minL<min(len.range)) {showNotification(paste0("Minimum observed size (", minL
    #     ,") is less than the selected minimum size (", min(len.range),")" ),id="minLid",duration=NULL,type="warning")
    # } else removeNotification(id="minLid")
    # if(maxL>max(len.range)) {showNotification(paste0("Maximum observed size (", maxL
    #     ,") is greater than the selected maximum size (", max(len.range),")" ),id="maxLid",duration=NULL,type="warning")
    # } else removeNotification(id="maxLid")
    if(length(unUsedStrata)>0) {showNotification(paste0(" The following strata had no observed catch during the selected years: "
                                                        ,unUsedStrata),duration=NULL,id="unusedid",type="warning")
    } else removeNotification(id="unusedid")
    if(length(UnSampledStrata)>0) {
      #CheckTows=paste0(" The following strata had no tows during: "
      #                ,paste(paste(UnSampledStrata,collapse=" , ")," \n ",collapse=""))
      lineBrk=" <br></br> "
      UnSampledStrata2=ifelse(UnSampledStrata%in%yrs, paste0(lineBrk,UnSampledStrata,":  "),UnSampledStrata )
      #print(str(UnSampledStrata))
      #print(UnSampledStrata)
      # CheckTows=apply(UnSampledStrata,1,FUN=function(x) paste(x,collapse=" , "))
      # CheckTows=paste(CheckTows,"\n",collapse="")
      CheckTows=shiny::HTML(paste0(" The following strata had no tows: ",paste0(UnSampledStrata2,collapse = " ")))
      #print((CheckTows))
      #shiny::tags$iframe(,scrolling="yes")
      showNotification2(CheckTows,duration=NULL,id="unTowedid",type="warning")
    } else removeNotification(id="unTowedid")   
    
    #   }) #end observe "run" event
    # 
    # #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # #                                       MAP section 
    # #______________________________________________________________________________________________________
    # observeEvent(input$runBtn,{ #if run button is pushed: #DRH not sure why we need 2 observe events??? 
    
    get_data <- reactive({ #This is the get data function for the map
      #Get user inputs
      strata <- input$mychooser$right
      strata <- ifelse(nchar(strata)<5,paste0('0', strata),strata) #fix for when this is run twice - it adds another 0 and causes many errors
      # S<-input$S #already done above 
      # H<-input$H
      # G<-input$G 
      #develop sql query for data using input criteria
      q.sta <- paste0("select id, cruise6, stratum, tow, station, shg, svvessel, svgear, est_year, est_month, est_day, ",
                      "substr(est_time,1,2) || substr(est_time,4,2) as time, towdur, dopdistb, dopdistw, avgdepth, ",
                      "area, bottemp, botsalin, decdeg_beglat, decdeg_beglon from union_fscs_svsta ",
                      "where cruise6 IN (", paste(c(fall.cruises,spring.cruises), collapse = ','), ")",
                      " and STRATUM IN ('", paste(strata, collapse = "','"), "')",
                      #" and shg<= '136'", 
                      " and STATYPE <= ",S," and HAUL <= ",H," and GEARCOND <= ", G, #changing this to allow user specified SHG choices
                      " and est_year between " , input$years[1], " AND ", input$years[2], " order by cruise6, stratum, tow, station")
      sta.view <- sqlQuery(sole,q.sta, as.is = c(TRUE, rep(FALSE,22))) 
      sta.view$SEASON <- rep('SPRING',NROW(sta.view))
      sta.view$SEASON[sta.view$CRUISE6 %in% fall.cruises] <- 'FALL'
      
      q.cat <- paste0("select id, cruise6, stratum, tow, station, svspp, catchsex, expcatchwt, expcatchnum from union_fscs_svcat ",
                      "where cruise6 IN (", paste(c(fall.cruises,spring.cruises), collapse = ','), ")",
                      " and STRATUM IN ('", paste(strata, collapse = "','"), "')",
                      "and svspp = ", species$SVSPP[species$COMNAME==input$species], 
                      " order by cruise6, stratum, tow, station", sep = '')
      cat.view <- sqlQuery(sole,q.cat, as.is = c(TRUE, rep(FALSE,8)))
      catch.data <- merge(sta.view, cat.view, by = 'ID',  all.x = T, all.y=F)
      catch.data$EXPCATCHNUM[is.na(catch.data$EXPCATCHNUM)] <- 0
      catch.data$EXPCATCHWT[is.na(catch.data$EXPCATCHWT)] <- 0
      bins.points <- c(10,25,50,75,100000000000)    # arbitrary decisions regarding catch/tow bins for right panel plot - "<5","5-10","10-50","50-100",">100"
      catch.data$bubbleSize = rep(NA,NROW(catch.data)) 
      catch.data$bubbleSize[catch.data$EXPCATCHNUM>0] <- col.bin(catch.data$EXPCATCHNUM[catch.data$EXPCATCHNUM>0], bins.points[1:5])
      catch.data$bubbleSize <- 1+(catch.data$bubbleSize-1)/2     # rescaling the size of the points
      
      catch.data <- catch.data[catch.data$SEASON %in% input$season,]
      print(names(catch.data))
      #print(table(catch.data$SEASON))  
      return(catch.data)
      
    }) #end of get_data
    
    foundational.map <- reactive({
      #builds the map from sql query and inputs
      catch.data <- get_data()
      # print(table(catch.data$SEASON))
      leaflet() %>% # create a leaflet map widget
        addProviderTiles("CartoDB.Positron") %>%
        setView(-71, 40, 6) %>% # map location
        addPolylines(data = Strata, color = "grey", weight = 1.5) %>%
        addCircles(color='navy',catch.data$DECDEG_BEGLON[catch.data$EXPCATCHNUM>0],catch.data$DECDEG_BEGLAT[catch.data$EXPCATCHNUM>0],
                   popup = paste("Year: ",catch.data$EST_YEAR[catch.data$EXPCATCHNUM>0], "<br>",
                                 "Station: ",catch.data$STATION.x[catch.data$EXPCATCHNUM>0], "<br>",
                                 "Stratum: ",catch.data$STRATUM.x[catch.data$EXPCATCHNUM>0], "<br>",
                                 "Tow: ",catch.data$TOW.x[catch.data$EXPCATCHNUM>0], "<br>",
                                 "Catch Wt (kg): ",catch.data$EXPCATCHWT[catch.data$EXPCATCHNUM>0], "<br>",
                                 "Catch Num: ",catch.data$EXPCATCHNUM[catch.data$EXPCATCHNUM>0], "<br>",
                                 "Season: ",catch.data$SEASON[catch.data$EXPCATCHNUM>0]),
                   stroke = FALSE, fillOpacity = 0.4,
                   radius = catch.data$bubbleSize[catch.data$EXPCATCHNUM>0]*5000)
      
    })
    
    #print(str(foundational.map()))
    
    output$myMap = leaflet::renderLeaflet({
      foundational.map() #This just calls the reactive map
    })
    
    # store the current user-created version
    # of the Leaflet map for download in 
    # a reactive expression
    user.created.map <- reactive({
      # call the foundational Leaflet map
      foundational.map() %>%
        # store the view based on UI
        setView( lng = input$map_center$lng
                 ,  lat = input$map_center$lat
                 , zoom = input$map_zoom
        )
    }) # end of creating user.created.map()
    #}) #end of observeEvent 
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    #                                       Output handling section 
    #______________________________________________________________________________________________________
    
    
    output$downloadData <- downloadHandler(
      filename = function() { paste(input$species, input$season, input$len1[1], input$len1[2], '.csv', sep='_') },
      content = function(file) {
        
        #All.out is bound to the reactive variables and assigned from within the observe event environment with "<<-"
        #so it should be visible here.
        #print(str(All.out))
        #Huge pain in the ass to get this to print the names of the list objects... 
        write.list=function(x) {
          write.table(x,file,append=T,sep=",",row.names = F,col.names = F)
          write.table(data.frame(All.out[[x]]),file,row.names = F,append= T,sep=',' )
        }
        suppressWarnings(lapply(names(All.out),write.list))
        
        write.list=function(x) {
          write.table(x,file,append=T,sep=",",row.names = F,col.names = F)
          write.table(data.frame(userInputs[[x]]),file,row.names = F,append= T,sep=',' ,col.names = F)
        }
        suppressWarnings(lapply(names(userInputs),write.list))
        
        #Add data to the end of this output
        write.table("CatchData",file,row.names = F,append= T,sep=',' ,col.names = F)
        write.table(CatchData,file,row.names = F,append= T,sep=',' ,col.names = T)
        
      }
    )
    
    output$downloadDataR <- downloadHandler(
      filename = function() { paste(input$species, input$season,input$len1[1],input$len1[2], '.RData', sep='_') },
      content = function(file) {
        
        SAGAr=list("Indices"=All.out,"UserInputs"=userInputs,"Catch.Data"=CatchData)
        #SAGAr=list("Indices"=All.out)
        save(SAGAr,file=file)
        
      }
    )
    
    # output$downloadMap <- downloadHandler(
    #   filename = function() { paste("SurveyMap",input$species, input$season, min(input$minLength), max(input$maxLength), '.pdf', sep='_') },
    #   content = function(file) {
    #     mapview::mapshot( x = user.created.map()
    #              , file = file
    #              , cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
    #              , selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
    #     )  
    #     
    #   }
    # )  
    
    output$downloadMapHTML <- downloadHandler(
      
      filename = function() { 
        showNotification("Downloading map...",duration=5,id="downloadmap",type="message")
        paste("SurveyMap",input$species, input$season
              , input$len1[1],input$len1[2], '.html', sep='_') },
      content = function(file) {
        htmlwidgets::saveWidget(
          widget = foundational.map()
          , file = file
        )
        
      }
      
    )
    
    output$text <- renderUI({
      str1 <- paste("Species:", input$species)
      str2 <- paste("Years:",
                    input$years[1], "to", input$years[2])
      str3 <- paste("Season:", input$season)
      str4 <- paste("Strata:", paste(input$mychooser$right, collapse=', '))
      HTML(paste(str1, str2, str3, str4, sep = '<br/>'))
      
    })
    # htmlwidgets::saveWidget(output$myMap, "temp.html", selfcontained = FALSE)
    #  webshot::webshot("temp.html", file = paste0(Sys.time(),"_map.png"))   
    
  }) #end of observeEvent  #DRH - I put everything inside the observe event and now the download handlers can see all the arguments
}
shinyApp(ui, server)