library(leaflet)
library(rgeos)
library(rgdal)
library(shiny)
library(RODBC)
library(shinydashboard)
library(webshot)
library(htmlwidgets)
library(ggplot2)

Sys.setenv(ORACLE_HOME="/ora1/app/oracle/product/11.2.0/dbhome_1")
sole <- odbcConnect(dsn="sole", uid="amiller", pwd="$$Vwda44862003", believeNRows=FALSE)
#nova <- odbcConnect(dsn="nova", uid="amiller", pwd="$$Vwda44862003", believeNRows=FALSE)
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

input=c()
input$strata=c('1260','1270','1280','1370')
input$years2=c(2009,2009)
input$species="ATLANTIC COD"
input$season="FALL"
cruise6 <- survey.cruises$CRUISE6[survey.cruises$SEASON == input$season & 
                                    survey.cruises$YEAR %in% seq(min(input$years2),max(input$years2))]
spp <- species$SVSPP[species$COMNAME == input$species] #species name as well
#strata.in = paste(input$strata, collapse = "','")
strata.in <- input$strata
spp=spp;survey = cruise6;oc = sole;strata = strata.in;lengths = 50:60;do.length = TRUE;
do.age = FALSE;gcf = 1;dcf = 1;vcf = 1;tow_swept_area = 0.01













