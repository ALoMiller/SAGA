library(leaflet)
library(rgeos)
library(rgdal)
library(shiny)
library(RODBC)
library(shinydashboard)
library(webshot)
library(htmlwidgets)
library(ggplot2)
library(sf)
library(mapview)

#webshot::install_phantomjs()

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
        sidebarMenu(style = "position: fixed; overflow: visible;",
          menuItem(
            "Survey Indices", 
            tabName = "indices", 
            icon = icon("globe")
            ),
          menuItem(
            "Maps", 
            tabName = "maps", 
            icon = icon("globe")
            )
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "indices",                        #SAGA clone application user options 
              fluidRow(
                column(6,
                      chooserInput("mychooser", "Available frobs", "Selected frobs", #new custom widget strata selection using chooser.R
                        strata.list[,1], c(), size = 20, multiple = TRUE
                      ),
                       #verbatimTextOutput("selection"),
                       #selectInput("strata", "Select strata:",                #strata selection (switch to custom widget)
                      #             choices=strata.list[,1],
                      #             multiple=TRUE),
                       
                       #This is another option for strata selection...
                       # br(),
                       # pickerInput(
                       #   inputId = "mychooser",
                       #   label = "Strata",
                       #   choices = strata.list[,1],
                       #   multiple = TRUE,
                       #   options = list(
                       #    `actions-box` = TRUE,
                       #    `deselect-all-text` = "Deselect",
                       #    `select-all-text` = "All strata",
                       #    `none-selected-text` = "zero"
                       #   )
                       # ),
                       selectInput("species", "Select species:",              #Species drop menu
                                   choices =  species$COMNAME, 
                                   selected = "BLACK SEA BASS"),
                       
                       radioButtons("season", "Choose season:",               #species radio buttons - switch map check boxes to these?  Probably a good idea.
                                    choices = list("SPRING" = "SPRING", "FALL" = "FALL"), 
                                    selected = "SPRING"),
                       
                       sliderInput("years", "Select range of year(s)",            #Years slider
                                  min = 1950, 
                                  max = 2019,
                                  value = c(2009,2018), sep = ""),
                       h5(strong("Enter range of lengths:")),             #length input (switch to dynamic options)
                       fluidRow(
                            column(3,
                                  textInput("minLength", 
                                            label = NULL, 
                                            value = "0", 
                                            width = "100")),
                            column(3,
                                  textInput("maxLength", 
                                            label = NULL, 
                                            value = "100", 
                                            width = "100"))
                         ),
                       checkboxInput("BigelowCalib", label = "Apply Bigelow Calibration", value = FALSE),
                  
                      h5(strong("SHG values")),
                      fluidRow(
                        column(4,
                               textInput("S", 
                                         label = "Sta. <=", 
                                         value = "1", 
                                         width = "50px")),
                        column(4,
                              textInput("H", 
                                  label = "Haul <=" , 
                                  value = "3", 
                                  width = "50px")),
                        column(4,
                               textInput("G", 
                                         label = "Gear <=", 
                                         value = "6", 
                                         width = "50px"))
                      ),
                       #Option to run current settings
                       actionButton("runBtn","RUN"),
                       br(),
                       br(),
                       #download data
                       downloadButton('downloadData', 'Download .csv Data'),
                       br(),
                       br(),
                       #download data2
                       downloadButton('downloadDataR', 'Download RData')
                       
                ),
                column(7,
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
                downloadButton('downloadMap', 'Download Map (.pdf)'),
                br(),
                br(),
                downloadButton('downloadMapHTML', 'Download Map (.html)'),
                br(),
                br(),
                h2("User Inputs"),
                verbatimTextOutput( outputId = "text")
              )  
              # ), 
              # column(width = 3,
              #        selectInput("species", "Select species:",                  #Species drop menu
              #                    choices =  species$COMNAME, 
              #                    selected = "BLACK SEA BASS"),
              #        sliderInput("years", "Select range of year(s)",            #Years slider
              #                    min = 1962, 
              #                    max = 2018,
              #                    value = c(2014,2016), sep = ""),
              #        checkboxGroupInput("season",                              #Season check box
              #                           label = "Select Season(s)", 
              #                           choices = list("SPRING" = "SPRING", "FALL" = "FALL"),
              #                           selected = "SPRING")
              # )
            )
          )   
       )
    )
  )
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
server = function(input, output, session){
  source("helper.R")  #moved the stratification calculation function out the server function for ease of reading the code
  #it is now in helper.R
  output$selection <- renderPrint(
    input$mychooser$right
  )
<<<<<<< HEAD
  observeEvent(input$runBtn,{ #if run button is pushed:
=======
  # print list of input events
  output$text <- renderPrint({
    #reactiveValuesToList(input[which(substr(names(input),1,4)=="map_")])
    reactiveValuesToList(input)[c("species","years","season","mychooser")] 
    #,input$mychooser$right,input$years,input$season,input$S,input$H,input$G))
    })
  
  observeEvent(input$runBtn,{ #if run button is pushed:
    #print(c("mycheck",length(input$mychooser$right)))
    req(input$mychooser$right)
    
    
>>>>>>> a676b707fc6ec4ad8f2a6751044dbbdb57b324f8
    #grab cruise6 from the rows with matching season and year
    cruise6 <- survey.cruises$CRUISE6[survey.cruises$SEASON == input$season & 
                                        survey.cruises$YEAR %in% seq(min(input$years),max(input$years))]
    spp <- species$SVSPP[species$COMNAME == input$species] #species name as well
    #strata.in = paste(input$strata, collapse = "','")
    #strata.in <- input$strata #the strata selected by the user
    
    strata.in <- input$mychooser$right
    #strata.in <- input$mychooser
    len.range <- c(input$minLength:input$maxLength)
    do.Albatross <- input$BigelowCalib
    #Check user inputs
    S<-input$S
    H<-input$H
    G<-input$G
    #Check user inputs and make a table of them for later 
    print(strata.in)
    print(seq(min(input$years),max(input$years)))
    print(cruise6)
    print(spp)
    print(len.range)
    #Check length range given user inputs (there is no reason to generate rows of zeros!):
    q.len <- paste("select MIN(length) as minL, MAX(length) as maxL  from svdbs.union_fscs_svlen " ,
                   " where cruise6 in ('", paste(cruise6, collapse="','"), "')"
                   , " and STRATUM IN('", paste(ifelse(nchar(strata.in)<5,paste0('0', strata.in),strata.in), collapse = "','")
                   , "')"," and svspp = ", spp, ";", sep = '')
    print(q.len)
    len.view <- as.data.frame(sqlQuery(sole,q.len))
    #adjust length range and report in console
    if(min(len.range)<len.view$MINL | max(len.range)>len.view$MAXL) print("Adjusting user requested lengths to range of observed values")
    if(min(len.range)<len.view$MINL) len.range <- len.range[which(len.range>=len.view$MINL)]
    if(max(len.range)>len.view$MAXL) len.range <- len.range[which(len.range<=len.view$MAXL)]
    if(input$minLength<len.view$MINL | input$maxLength>len.view$MAXL) print(paste("New range: ",len.range))
    
    #print(survey.cruises$CRUISE6[survey.cruises$SEASON == input$season ])
    #print(survey.cruises$CRUISE6[survey.cruises$YEAR %in% seq(min(input$years),max(input$years))])
    userInputs=list("Species"=input$species,"Strata"=strata.in,"Years"=seq(min(input$years),max(input$years))
                    ,"Season"=input$season,"Lengths"=len.range)
    dput(userInputs,"user.Inputs") #other environments can see this after reading 
    
    if(length(cruise6)>0){
      Ind.out=c();IAL.out=c();VIAL.out=c();
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
                                                  do.Albatross = do.Albatross,
                                                  tow_swept_area = 0.01,
                                                  S=S,H=H,G=G)
        #print(names(x.out))
        #print(class(x.out))
        #print(str(x.out))
        #print(x.out$out)
        #print(x.out)
        #print(x.out$Nal.hat.stratum[1:3,])
        
        #Take the important parts from x.out to generate an index over time.
        Yeari=as.integer(substr(paste(cruise6[i]),1,4))
        Tows=sum(x.out$out[,"m"]) #number of tows in the year in question
        
        #Generate products for later download and plotting:
        #grab the Num,Wt and generate stratified means 
        SMns=colSums(x.out$out[,c(4:5)][!is.na(x.out$out[,4]),]*
                       x.out$out[,"M"][!is.na(x.out$out[,4])])/sum(x.out$out[,"M"][!is.na(x.out$out[,4])])
        #Now get the variances
        Svars=colSums(x.out$out[,c(6:7)][!is.na(x.out$out[,4]),]*
                        (x.out$out[,"M"][!is.na(x.out$out[,4])]^2))/sum(x.out$out[,"M"][!is.na(x.out$out[,4])])^2
        Ind.out<-rbind(Ind.out,c(Yeari,Tows
                    ,SMns,Svars)) 
        #The indices at length require similar manipulation
        #IAL.out<-rbind(IAL.out,c(Yeari,Tows,colSums(x.out$Nal.hat.stratum/x.out$out[,"M"])
        #    ,"Total"=sum(colSums(x.out$Nal.hat.stratum/x.out$out[,"M"]))))  #Add a "Total" which is the index over the sizes of interest
        IAL.out<-rbind(IAL.out,c(Yeari,Tows,colSums(x.out$Nal.hat.stratum/sum(x.out$out[,"M"]))
                                 ,"Total"=sum(colSums(x.out$Nal.hat.stratum/sum(x.out$out[,"M"])))))  #Add a "Total" which is the index over the sizes of interest
        
        #divide by the stratum area to get unexpanded numbers at length
        VIAL.out<-rbind(VIAL.out,c(Yeari,Tows,colSums(x.out$V.Nal.stratum/sum(x.out$out[,"M"])^2)
              ,"Total"=sum(colSums(x.out$V.Nal.stratum/sum(x.out$out[,"M"])^2)))) #remove stratum area expansion
      }
      Ind.out=as.data.frame(Ind.out)
      names(Ind.out)=c("Year","Tows","Num","Wt","VarNum","VarWt")
      dput(Ind.out,"Ind.out") #This should make this visible to other environments for later download
      IAL.out=as.data.frame(IAL.out)
      names(IAL.out)=c('Year','nTows',paste(len.range,"cm",sep=""),'Total')
      dput(IAL.out,"IAL.out") #This should make this visible to other environments for later download
      VIAL.out=as.data.frame(VIAL.out)
      names(VIAL.out)=c('Year','nTows',paste(len.range,"cm",sep=""),'Total')
      dput(VIAL.out,"VIAL.out") #This should make this visible to other environments for later download
   
      #print(IAL.out)
      #print(VIAL.out)
      
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
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #                                       MAP section 
  #______________________________________________________________________________________________________
  get_data <- reactive({ #This is the get data button
    #Get user inputs
    strata <- input$mychooser$right
    #strata <- input$mychooser
    strata <- ifelse(nchar(strata)<5,paste0('0', strata),strata) #fix for when this is run twice - it adds another 0 and causes many errors
    S<-input$S
    H<-input$H
    G<-input$G 
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
    #print(table(sta.view$SEASON))  
    #print(head(sta.view))
    q.cat <- paste0("select id, cruise6, stratum, tow, station, svspp, catchsex, expcatchwt, expcatchnum from union_fscs_svcat ",
                    "where cruise6 IN (", paste(c(fall.cruises,spring.cruises), collapse = ','), ")",
                    " and STRATUM IN ('", paste(strata, collapse = "','"), "')",
                    "and svspp = ", species$SVSPP[species$COMNAME==input$species], 
                    " order by cruise6, stratum, tow, station", sep = '')
    cat.view <- sqlQuery(sole,q.cat, as.is = c(TRUE, rep(FALSE,8)))
    #print(head(cat.view))
    catch.data <- merge(sta.view, cat.view, by = 'ID',  all.x = T, all.y=F)
    #print(head(catch.data))
    catch.data$EXPCATCHNUM[is.na(catch.data$EXPCATCHNUM)] <- 0
    catch.data$EXPCATCHWT[is.na(catch.data$EXPCATCHWT)] <- 0
    bins.points <- c(10,25,50,75,100000000000)    # arbitrary decisions regarding catch/tow bins for right panel plot - "<5","5-10","10-50","50-100",">100"
    catch.data$bubbleSize = rep(NA,NROW(catch.data)) 
    catch.data$bubbleSize[catch.data$EXPCATCHNUM>0] <- col.bin(catch.data$EXPCATCHNUM[catch.data$EXPCATCHNUM>0], bins.points[1:5])
    catch.data$bubbleSize <- 1+(catch.data$bubbleSize-1)/2     # rescaling the size of the points
    
    catch.data <- catch.data[catch.data$SEASON %in% input$season,]
    #print(input$season)
    #print(table(catch.data$SEASON))  
    return(catch.data)
    
  })
  
  foundational.map <- reactive({
    #builds the map from sql query and inputs
    catch.data <- get_data()
    # print(table(catch.data$SEASON)) 
    leaflet() %>% # create a leaflet map widget
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
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #                                       Output handling section 
  #______________________________________________________________________________________________________
  #Get the saved output objects
  getOutput=reactive({
    #This is a way to get the download handler to see these values... (from an observe event above)
    if(file.exists("Ind.out")) Ind.out=dget("Ind.out")
    if(file.exists("IAL.out")) IAL.out=dget("IAL.out")
    if(file.exists("VIAL.out")) VIAL.out=dget("VIAL.out")
    if(all(c("VIAL.out","IAL.out","Ind.out")%in%objects())) {
      All.out=list("Index"=(Ind.out),"NatLength"=(IAL.out), "VarNatLength"=(VIAL.out))
      #print(names(All.out))
    }
  })
  
  getInputs=reactive({
    if(file.exists("user.Inputs")) user.Inputs=dget("user.Inputs")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$species, input$season, min(input$minLength), max(input$maxLength), '.csv', sep='_') },
    content = function(file) {

      All.out=getOutput()
    
      #Huge pain in the ass to get this to print the names of the list objects... 
      write.list=function(x) {
        write.table(x,file,append=T,sep=",",row.names = F,col.names = F)
        write.table(data.frame(All.out[[x]]),file,row.names = F,append= T,sep=',' )
      }
      suppressWarnings(lapply(names(All.out),write.list))
      
      # if(file.exists("user.Inputs")) user.Inputs=dget("user.Inputs")
      user.Inputs=getInputs()
      write.list=function(x) {
        write.table(x,file,append=T,sep=",",row.names = F,col.names = F)
        write.table(data.frame(user.Inputs[[x]]),file,row.names = F,append= T,sep=',' ,col.names = F)
      }
      suppressWarnings(lapply(names(user.Inputs),write.list))
     
    }
  )
  
  output$downloadDataR <- downloadHandler(
    filename = function() { paste(input$species, input$season, min(input$minLength), max(input$maxLength), '.Rdata', sep='_') },
    content = function(file) {
      
      All.out=getOutput()
      user.Inputs=getInputs()
      
      SAGAr=list("Indices"=All.out,"UserInputs"=user.Inputs)
      save(SAGAr,file=file)
      
    }
  )
  
  output$downloadMap <- downloadHandler(
    filename = function() { paste("SurveyMap",input$species, input$season, min(input$minLength), max(input$maxLength), '.pdf', sep='_') },
    content = function(file) {
      mapview::mapshot( x = user.created.map()
               , file = file
               , cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
               , selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
      )  
      
    }
  )  
  
  output$downloadMapHTML <- downloadHandler(
    filename = function() { paste("SurveyMap",input$species, input$season, min(input$minLength), max(input$maxLength), '.html', sep='_') },
    content = function(file) {
      htmlwidgets::saveWidget(
         widget = foundational.map()
         , file = file
       )
      
    }
  )  
  
  # htmlwidgets::saveWidget(output$myMap, "temp.html", selfcontained = FALSE)
  #  webshot::webshot("temp.html", file = paste0(Sys.time(),"_map.png"))   
}
shinyApp(ui, server)
