library(leaflet)
library(rgeos)
library(rgdal)
library(shiny)
library(RODBC)
library(shinydashboard)
library(webshot)
library(htmlwidgets)
library(ggplot2)

source("chooser.R") 
Sys.setenv(ORACLE_HOME="/ora1/app/oracle/product/11.2.0/dbhome_1")
species <- read.csv('files/speciesTable.csv')
spp.list <- split(species$SVSPP,species$COMNAME)
strata.list <- read.csv('files/StrataList.csv')
strata.list$YourStrata <- as.character(strata.list$YourStrata)
saved.strata <- strata.list$AllStrata[1:5]
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

get.survey.data.fn <- function(oc = sole, purpose.code = 10){
  q.surveys <- paste0("select cruise6,purpose_code,season,year, startdate, enddate from mstr_cruise where purpose_code=", purpose.code, 
                      " and cruise6 is not null order by year,cruise6,season")
  survey.view <- sqlQuery(oc,q.surveys)
  return(survey.view)
}
survey.cruises <- get.survey.data.fn()
fall.cruises <- unique(survey.cruises$CRUISE6[survey.cruises$SEASON == 'FALL'])
spring.cruises <- unique(survey.cruises$CRUISE6[survey.cruises$SEASON == 'SPRING'])


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ui <-
  dashboardPage(
    dashboardHeader(title = "NEFSC Survey Data Portal"), #"Portal" is not showing up in the sidebar
      dashboardSidebar(
        #Port user between options for mapping application or SAGA clone
        sidebarMenu(
          menuItem(
            "Maps", 
            tabName = "maps", 
            icon = icon("globe")),
          menuItem(
            "Survey Indices", 
            tabName = "indices", 
            icon = icon("globe"))
          )
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "maps", #Define Mapping app options
              fluidRow(
                column(width = 9,
                     leafletOutput("myMap", height = "91.5vh")
                ), 
                column(width = 3,
                     selectInput("species", "Select species:",                  #Species drop menu
                                 choices =  species$COMNAME, 
                                 selected = "BLACK SEA BASS"),
                     sliderInput("years", "Select range of year(s)",            #Years slider
                                 min = 1962, 
                                 max = 2018,
                                 value = c(2014,2016), sep = ""),
                     checkboxGroupInput("season",                              #Season check box
                                        label = "Select Season(s)", 
                                        choices = list("SPRING" = "SPRING", "FALL" = "FALL"),
                                        selected = "SPRING")
                )
              )
            ),
        tabItem(
          tabName = "indices",                        #SAGA clone application user options 
            fluidRow(
              column(4,
                     chooserInput("mychooser", "Available frobs", "Selected frobs", #new custom widget strata selection using chooser.R
                       strata.list[,1], c(), size = 10, multiple = TRUE
                     ),
                     #verbatimTextOutput("selection"),
                     #selectInput("strata", "Select strata:",                #strata selection (switch to custom widget)
                    #             choices=strata.list[,1],
                    #             multiple=TRUE),
                     
                     selectInput("species2", "Select species:",              #Species drop menu
                                 choices =  species$COMNAME, 
                                 selected = "BLACK SEA BASS"),
                     
                     radioButtons("season2", "Choose season:",               #species radio buttons - switch map check boxes to these?  Probably a good idea.
                                  choices = list("SPRING" = "SPRING", "FALL" = "FALL"), 
                                  selected = "SPRING"),
                     
                     sliderInput("years2", "Select range of year(s)",            #Years slider
                                min = 1950, 
                                max = 2019,
                                value = c(2009,2018), sep = ""),
                     h5(strong("Enter range of lengths:")),             #length input (switch to dynamic options)
                       fluidRow(
                          column(6,
                                textInput("minLength", 
                                          label = NULL, 
                                          value = "0", 
                                          width = "100")),
                          column(6,
                                textInput("maxLength", 
                                          label = NULL, 
                                          value = "100000", 
                                          width = "100"))
                       ),
                     checkboxInput("BigelowCalib", label = "Apply Bigelow Calibration", value = FALSE),
                
                     #Option to run current settings
                     actionButton("runBtn","RUN"),
                     br(),
                     br(),
                     #download data
                     downloadButton('downloadData', 'Download Data')
                     
              ),
              column(7,
                plotOutput("myPlots")
                )
            
          )
          # Show a plot of the generated survey indices by strata 
          #tabPanel("N (all sizes and ages) by strata",
           #      plotOutput("myPlots")
          #)
        )
      )
    )
  )
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
server = function(input, output, session){
  source("helper.R")  #moved the stratification calculation function out the server function for ease of reading the code
  #it is now in helper.R
  output$selection <- renderPrint(
    input$mychooser
  )
  observeEvent(input$runBtn,{ #if run button is pushed:
    #grab cruise6 from the rows with matching season and year
    cruise6 <- survey.cruises$CRUISE6[survey.cruises$SEASON == input$season2 & 
                                        survey.cruises$YEAR %in% seq(min(input$years2),max(input$years2))]
    spp <- species$SVSPP[species$COMNAME == input$species2] #species name as well
    #strata.in = paste(input$strata, collapse = "','")
    #strata.in <- input$strata #the strata selected by the user
    strata.in <- input$mychooser$right
    len.range <- c(input$minLength:input$maxLength)
    do.Bigelow <- input$BigelowCalib
    #Check user inputs
    print(strata.in)
    print(seq(min(input$years2),max(input$years2)))
    print(cruise6)
    print(spp)
    print(len.range)
    #print(survey.cruises$CRUISE6[survey.cruises$SEASON == input$season2 ])
    #print(survey.cruises$CRUISE6[survey.cruises$YEAR %in% seq(min(input$years2),max(input$years2))])
    
    if(length(cruise6)>0){
      Ind.out=c();IAL.out=c();
      for(i in 1:length(cruise6)) {
        x.out<- get.survey.stratum.estimates.2.fn(spp=spp,
                                                  survey = cruise6[i], 
                                                  oc = sole, 
                                                  strata = strata.in,
                                                  lengths = len.range, 
                                                  do.length = TRUE, 
                                                  do.age = FALSE, 
                                                  gcf = 1, 
                                                  dcf = 1, 
                                                  vcf = 1, 
                                                  do.Bigelow = do.Bigelow
                                                  tow_swept_area = 0.01)
        #print(names(x.out))
        #print(class(x.out))
        #print(str(x.out))
        #print(x.out$out)
        #print(x.out)
        #print(x.out$Nal.hat.stratum[1:3,])
        
        #Take the important parts from x.out to generate an index over time.
        Yeari=as.integer(substr(paste(cruise6[i]),1,4))
        Tows=sum(x.out$out[,"m"]) #number of tows in the year in question
        Ind.out<-rbind(Ind.out,c(Yeari,Tows,colSums(x.out$out[,c(4:7)][!is.na(x.out$out[,4]),]))) #grab the Num,Wt,varNum,VarWt
        
        IAL.out<-rbind(IAL.out,c(Yeari,Tows,colSums(x.out$Nal.hat.stratum)))
        
      }
      Ind.out=as.data.frame(Ind.out)
      names(Ind.out)=c("Year","Tows","Num","Wt","VarNum","VarWt")
      #plot the indices for something to look at after a successful run
      if(!is.na(x.out[[2]][1,1])) { #Check to make sure x.out was loaded before attempting to plot
        output$myPlots <- renderPlot({
          
          #plot1 <- ggplot(as.data.frame(x.out$out), aes(x=stratum, y= EXPCATCHNUM)) +
          #    geom_bar(stat="identity") +
          # theme_bw()
          if(nrow(Ind.out)>1){ #make sure there is more than one year to plot
            #calculate a confidence interval to add to plot
            ci=data.frame(Ind.out,"lci"=Ind.out$Wt-1.96*(sqrt(Ind.out$VarWt)/Ind.out$Tows)
                          ,"uci"=Ind.out$Wt+1.96*(sqrt(Ind.out$VarWt)/Ind.out$Tows))
            
            print(ci)
            
            plot1<-ggplot(Ind.out, aes(x=Year, y=Wt)) +
                 geom_line() +
                 geom_ribbon(data=ci,aes(ymin=lci,ymax=uci),alpha=0.3) +
                 theme_bw()
  
            print(plot1)
          }
        })
      } else print("Invalid Stratum Selection: no observations of selected species in strata")
    } else print("Invalid Stratum Selection: no observations of selected species in strata")
  }) #end observe "run" event
  
  get_data <- reactive({ #This is the get data button
    #develop sql query for data using input criteria
    q.sta <- paste0("select id, cruise6, stratum, tow, station, shg, svvessel, svgear, est_year, est_month, est_day, ",
                    "substr(est_time,1,2) || substr(est_time,4,2) as time, towdur, dopdistb, dopdistw, avgdepth, ",
                    "area, bottemp, botsalin, decdeg_beglat, decdeg_beglon from union_fscs_svsta ",
                    "where cruise6 IN (", paste(c(fall.cruises,spring.cruises), collapse = ','), ")",
                    " and shg<= '136' and est_year between " , input$years[1], " AND ", input$years[2], " order by cruise6, stratum, tow, station")
    sta.view <- sqlQuery(sole,q.sta, as.is = c(TRUE, rep(FALSE,22))) 
    sta.view$SEASON <- rep('SPRING',NROW(sta.view))
    sta.view$SEASON[sta.view$CRUISE6 %in% fall.cruises] <- 'FALL'
    print(table(sta.view$SEASON))  
    print(head(sta.view))
    q.cat <- paste0("select id, cruise6, stratum, tow, station, svspp, catchsex, expcatchwt, expcatchnum from union_fscs_svcat ",
                    "where cruise6 IN (", paste(c(fall.cruises,spring.cruises), collapse = ','), ")",
                    "and svspp = ", species$SVSPP[species$COMNAME==input$species], " order by cruise6, stratum, tow, station", sep = '')
    cat.view <- sqlQuery(sole,q.cat, as.is = c(TRUE, rep(FALSE,8)))
    print(head(cat.view))
    catch.data <- merge(sta.view, cat.view, by = 'ID',  all.x = T, all.y=F)
    print(head(catch.data))
    catch.data$EXPCATCHNUM[is.na(catch.data$EXPCATCHNUM)] <- 0
    catch.data$EXPCATCHWT[is.na(catch.data$EXPCATCHWT)] <- 0
    bins.points <- c(10,25,50,75,100000000000)    # arbitrary decisions regarding catch/tow bins for right panel plot - "<5","5-10","10-50","50-100",">100"
    catch.data$bubbleSize = rep(NA,NROW(catch.data)) 
    catch.data$bubbleSize[catch.data$EXPCATCHNUM>0] <- col.bin(catch.data$EXPCATCHNUM[catch.data$EXPCATCHNUM>0], bins.points[1:5])
    catch.data$bubbleSize <- 1+(catch.data$bubbleSize-1)/2     # rescaling the size of the points
    
    catch.data <- catch.data[catch.data$SEASON %in% input$season,]
    print(input$season)
    print(table(catch.data$SEASON))  
    return(catch.data)
    
  })
  
  foundational.map <- reactive({
    
    leaflet() %>% # create a leaflet map widget
      addProviderTiles("CartoDB.Positron")
  })
  
  output$myMap = renderLeaflet({
    catch.data <- get_data()
    print(table(catch.data$SEASON)) 
    foundational.map() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-71, 40, 6) %>% # map location
      addPolylines(data = Strata, color = "grey", weight = 1.5) %>%
      addCircles(color='navy',catch.data$DECDEG_BEGLON[catch.data$EXPCATCHNUM>0],catch.data$DECDEG_BEGLAT[catch.data$EXPCATCHNUM>0],
                 popup = paste("Station: ",catch.data$STATION.x[catch.data$EXPCATCHNUM>0], "<br>",
                               "Stratum: ",catch.data$STRATUM.x[catch.data$EXPCATCHNUM>0], "<br>",
                               "Tow: ",catch.data$TOW.x[catch.data$EXPCATCHNUM>0], "<br>",
                               "Catch Wt (kg): ",catch.data$EXPCATCHWT[catch.data$EXPCATCHNUM>0], "<br>",
                               "Catch Num: ",catch.data$EXPCATCHNUM[catch.data$EXPCATCHNUM>0], "<br>",
                               "Season: ",catch.data$SEASON[catch.data$EXPCATCHNUM>0]),
                 stroke = FALSE, fillOpacity = 0.4,
                 radius = catch.data$bubbleSize[catch.data$EXPCATCHNUM>0]*5000)  
    
  })
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$spp, input$survey, min(input$minLength), max(input$maxLength), '.csv', sep='_') },
    content = function(file) {
      lapply(x.out, function(x) write.table( data.frame(x), 'test.csv'  , append= T, sep=',' ))
      #write.csv(get_data()$obs.data, file)
    }
  )
  # htmlwidgets::saveWidget(output$myMap, "temp.html", selfcontained = FALSE)
  #  webshot::webshot("temp.html", file = paste0(Sys.time(),"_map.png"))   
}
shinyApp(ui, server)
