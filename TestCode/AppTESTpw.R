library(leaflet)
library(rgeos)
library(rgdal)
library(shiny)
library(RODBC)
library(shinydashboard)
library(webshot)
library(htmlwidgets)

Sys.setenv(ORACLE_HOME="/ora1/app/oracle/product/11.2.0/dbhome_1")


#need to get password and uid from user...
if(file.exists("~/sas_data.sas")) { #some users will have this file - which contains the uid and pw for sole and nova
  tmp=scan(file="~/sas_data.sas",what="character")
  tmp=tmp[which(!tmp%in%c("%let", "=",";"))] #drop some useless parts
  #need to eliminate some other variables that may not conform to 1 style
  tmp=tmp[-grep("ora",tmp)]
  uid=tmp[1];pwd=tmp[2]
  sole <- odbcConnect(dsn="sole", uid=uid, pwd=pwd, believeNRows=FALSE) #
  nova <- odbcConnect(dsn="nova", uid=uid, pwd=pwd, believeNRows=FALSE) #
} else {
  #need to get password and uid from user...
  sole <- odbcConnect(dsn="sole", believeNRows=FALSE) 
  nova <- odbcConnect(dsn="nova", believeNRows=FALSE) 
}
#sole <- odbcConnect(dsn="sole", believeNRows=FALSE) #, uid="amiller", pwd="$$Vwda44862003"
#nova <- odbcConnect(dsn="nova", believeNRows=FALSE) #, uid="amiller", pwd="$$Vwda44862003"  



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

#Strata <- readOGR("//155.206.139.2/home10/amiller/POPDY/AssessmentWork/R/Maps/shapefiles/strata","groundfish_strata") ## read sampling strata
Strata <- readOGR("//net/home10/amiller/POPDY/AssessmentWork/R/Maps/shapefiles/strata","groundfish_strata") ## read sampling strata

get.survey.data.fn <- function(oc = sole, purpose.code = 10){
  q.surveys <- paste0("select cruise6,purpose_code,season,year, startdate, enddate from mstr_cruise where purpose_code=", purpose.code, 
    " and cruise6 is not null order by year,cruise6,season")
  survey.view <- sqlQuery(oc,q.surveys)
  return(survey.view)
}
survey.cruises <- get.survey.data.fn()
fall.cruises <- unique(survey.cruises$CRUISE6[survey.cruises$SEASON == 'FALL'])
spring.cruises <- unique(survey.cruises$CRUISE6[survey.cruises$SEASON == 'SPRING'])

ui <-
  dashboardPage(
  dashboardHeader(title = "NEFSC Survey Data Portal"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Maps", 
        tabName = "maps", 
        icon = icon("globe")),
      menuItem(
        "Survey Indices", 
        tabName = "indices", 
        icon = icon("globe"))
  )),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "maps",
        fluidRow(
          column(width = 9,
          leafletOutput("myMap", height = "91.5vh")
            ), 
       column(width = 3,
          selectInput("species", "Select species:",
          choices =  species$COMNAME, selected = "BLACK SEA BASS"),
        sliderInput("years", "Select range of year(s)",
          min = 1962, max = 2018, value = c(2014,2016), sep = ""),
        checkboxGroupInput("season", label = "Select Season(s)", 
          choices = list("SPRING" = "SPRING", "FALL" = "FALL"),
          selected = "SPRING")
         )
        )),
      tabItem(
        tabName = "indices",
        fluidRow(
          column(4,
            selectInput("strata", "Select strata:", 
              choices=strata.list[,1],multiple=TRUE),
            selectInput("species", "Select species:",
              choices =  species$COMNAME, selected = "BLACK SEA BASS"),
            radioButtons("season", "Choose season:",
              choices = list("SPRING" = 1, "FALL" = 2), 
              selected = 1),
            textInput("year", "Enter year:",value=""),
            
            actionButton("saveBtn","Save")
          ),
          column(7,
            textOutput("ss")
            )
          
          )
        )
      )
    )
)

server = function(input, output){
  
 observeEvent(input$saveBtn,{
  #output$ss <-  renderText({
  #as.character(saved.strata$YourStrata[!is.na(saved.strata$YourStrata)])
   cruise6 <- survey.cruises$CRUISE6[survey.cruises$SEASON == input$season & survey.cruises$YEAR == input$year]
   spp <- species$SVSPP[species$COMNAME == input$species]
   strata = paste(input$strata, collapse = "','")
   print(as.character(input$strata))
   get.survey.stratum.estimates.2.fn <- function(spp, survey = cruise6, oc = sole, strata = strata,lengths = 1:34, do.length = F, 
     #do.age = do.length, 
     gcf = 1, dcf = 1, vcf = 1, tow_swept_area = 0.01){
     
     #gcf.butterfish <- 1/1.35
     #gcf.butterfish.se <- 0.15
     #tow.size <- 0.01 #sq. nm
     
     stratum.sizes.q <- paste("select stratum, stratum_area, strgrp_desc, stratum_name from svmstrata where STRATUM IN ('", 
       paste(strata, collapse = "','"), "')"," order by stratum", sep = '')
     str.size <- sqlQuery(oc,stratum.sizes.q)
     str.size$NTOWS <- str.size$STRATUM_AREA/tow_swept_area
     
     strata <- paste('0', as.character(sort(unique(str.size$STRATUM))), sep ='')
     
     #STATION
     q.sta <- paste("select cruise6, stratum, tow, station, shg, svvessel, svgear, est_year, est_month, est_day, ",
       "substr(est_time,1,2) || substr(est_time,4,2) as time, towdur, dopdistb, dopdistw, avgdepth, ",
       "area, bottemp, beglat, beglon from union_fscs_svsta ",
       "where cruise6 = ", survey, " and STRATUM IN ('", paste(strata, collapse = "','"), "')",
       " and shg<= '136' order by cruise6, stratum, tow, station", sep = '')
     sta.view <- sqlQuery(oc,q.sta) 
     temp <- str.size[match(sta.view$STRATUM, str.size$STRATUM),]
     sta.view <- cbind(sta.view, temp[,-1])
     
     #CATCH
     q.cat <- paste("select cruise6, stratum, tow, station, svspp, catchsex, expcatchwt, expcatchnum from union_fscs_svcat ",
       "where cruise6 = ", survey, " and STRATUM IN ('", paste(strata, collapse = "','"), "')",
       "and svspp = ", spp, " order by cruise6, stratum, tow, station", sep = '')
     cat.view <- sqlQuery(oc,q.cat)
     #cat.view$ID <- as.character(cat.view$ID)
     catch.data <- merge(sta.view, cat.view, by = c('CRUISE6','STRATUM','TOW','STATION'),  all.x = T, all.y=F)
     catch.data$EXPCATCHNUM[is.na(catch.data$EXPCATCHNUM)] <- 0
     catch.data$EXPCATCHWT[is.na(catch.data$EXPCATCHWT)] <- 0
     #gear conversion
     catch.data$EXPCATCHNUM[which(is.element(catch.data$SVGEAR, c(41,45)))] <- gcf * catch.data$EXPCATCHNUM[which(is.element(catch.data$SVGEAR, c(41,45)))]
     catch.data$EXPCATCHWT[which(is.element(catch.data$SVGEAR, c(41,45)))] <- gcf * catch.data$EXPCATCHWT[which(is.element(catch.data$SVGEAR, c(41,45)))]
     #door conversion
     catch.data$EXPCATCHNUM[which(catch.data$YEAR< 1985)] <- dcf * catch.data$EXPCATCHNUM[which(catch.data$YEAR< 1985)]
     catch.data$EXPCATCHWT[which(catch.data$YEAR< 1985)] <- dcf * catch.data$EXPCATCHWT[which(catch.data$YEAR< 1985)]
     #vessel conversion
     catch.data$EXPCATCHNUM[which(catch.data$SVVESSEL == 'DE')] <- vcf * catch.data$EXPCATCHNUM[which(catch.data$SVVESSEL == 'DE')]
     catch.data$EXPCATCHWT[which(catch.data$SVVESSEL == 'DE')] <- vcf * catch.data$EXPCATCHWT[which(catch.data$SVVESSEL == 'DE')]
     
     m <- sapply(str.size$STRATUM, function(x) sum(sta.view$STRATUM == x))
     M <- str.size$NTOWS
     samp.tot.n.w <- t(sapply(str.size$STRATUM, function(x) apply(catch.data[which(catch.data$STRATUM== x),c('EXPCATCHNUM','EXPCATCHWT')],2,sum)))
     #	print(cbind(str.size$STRATUM,M,m,samp.tot.n.w))
     S.n.w.stratum <- t(sapply(str.size$STRATUM, function(x) var(catch.data[which(catch.data$STRATUM== x),c('EXPCATCHNUM','EXPCATCHWT')])))
     #	print(cbind(M,m,S.n.w.stratum))
     N.W.hat.stratum <- M * samp.tot.n.w/m
     Vhat.N.W.hat.stratum <- M^2 * (1 - m/M) * S.n.w.stratum/m
     n.strata <- length(M)
     
     if(do.length){
       #LENGTH
       q.len <- paste("select  cruise6, stratum, tow, station, catchsex, length, expnumlen from union_fscs_svlen " ,
         "where cruise6 = ", survey, " and STRATUM IN('", paste(strata, collapse = "','"), "')",
         "and svspp = ", spp, " order by cruise6, stratum, tow, station, svspp, catchsex", sep = '')
       len.view <- sqlQuery(oc,q.len)
       len.data <- merge(catch.data, len.view, by = c('CRUISE6','STRATUM','TOW','STATION','CATCHSEX'),  all.x=T, all.y = F)
       len.data$EXPNUMLEN[is.na(len.data$EXPNUMLEN)] <- 0
       if(max(len.data$LENGTH, na.rm= T) > max(lengths)) warning(paste('max of lengths in length data = ', max(len.data$LENGTH, na.rm= T), ' whereas max of lengths given is ', max(lengths), sep = ''))
       
       samp.tot.nal <- sapply(lengths, function(x) sapply(str.size$STRATUM, function(y) sum(len.data$EXPNUMLEN[len.data$STRATUM == y & len.data$LENGTH == x],
         na.rm = TRUE)))
       Nal.hat.stratum <- M * samp.tot.nal/m
       #		print(Nal.hat.stratum)
       #		print(cbind(apply(Nal.hat.stratum,1,sum), N.W.hat.stratum))
       S.nal.stratum <- t(sapply(str.size$STRATUM, function(x){
         x.dat <- len.data[which(len.data$STRATUM == x),]
         if(dim(x.dat)[1]){
           nal.by.tow <- t(sapply(unique(x.dat$TOW), function(y){
             if(sum(x.dat$TOW == y)){
               nal.tow <- sapply(lengths, function(z) sum(x.dat$EXPNUMLEN[which(x.dat$LENGTH == z & x.dat$TOW == y)], na.rm = TRUE))
             }
             else nal.tow <- rep(0,length(lengths))
             return(nal.tow)
           }))
           cov.nal <- cov(nal.by.tow)
           return(cov.nal)
         }
         else return(matrix(NA,length(lengths),length(lengths)))
       }))
       Vhat.Nal.stratum <- M^2 * (1 - m/M) * S.nal.stratum/m
       
       
       if(do.age){
         #AGE
         q.age <- paste("select  cruise6, stratum, tow, station, sex, length, age, indwt, maturity from union_fscs_svbio ",
           "where cruise6 = ", survey, " and STRATUM IN('", paste(strata, collapse = "','"), "')",
           "and svspp = ", spp, " and age is not null order by cruise6, stratum, tow, station", sep = '')
         age.view <- sqlQuery(oc,q.age)
         age.data <- merge(len.data, age.view, by = c('CRUISE6','STRATUM','TOW','STATION','LENGTH'),  all.x = T, all.y=F)
       }
     }
     
     
     
     out <- cbind(stratum = str.size$STRATUM, M = M, m = m, mean.n.w.per.tow = N.W.hat.stratum/M, V.mean.n.w.per.tow = Vhat.N.W.hat.stratum/(M^2))
     out <- list(out = out)
     out$stratum.size <- str.size
     if(do.length) {
       out$Nal.hat.stratum = Nal.hat.stratum
       out$V.Nal.stratum = Vhat.Nal.stratum
     }
     if(do.age) out$age.data <- age.data
     return(out)
   }
   x<- get.survey.stratum.estimates.2.fn()
   print(x)
   
 })

  get_data <- reactive({
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

 # htmlwidgets::saveWidget(output$myMap, "temp.html", selfcontained = FALSE)
#  webshot::webshot("temp.html", file = paste0(Sys.time(),"_map.png"))   

}
  shinyApp(ui, server)