
#**************************************************************************************************************
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
sole <- RODBC::odbcConnect(dsn="sole", uid="amiller", pwd="$$Vwda44862003", believeNRows=FALSE)


#To use the user input section - just copy here the part after list(
input=list(
species = "HADDOCK", strata = c( #"01010", "01020", "01030", 
#"01040", "01050", "01060", "01070", "01080", "01090", "01100", 
#"01110", "01120", "01130", "01140", "01150", "01160", "01170", 
#"01180", "01190", "01200", "01210", "01220", "01230", "01240", "01250"
 "01260", "01270", "01280", "01290", "01300"            #, "01310" 
#"01320", "01330", "01340"
, "01360", "01370", "01380", "01390", "01400"
#, "01610", "01620", "01630", "01640", "01650", "01660", 
#"01670", "01680", "01690", "01700", "01710", "01720", "01730", 
#"01740", "01750", "01760"
), years = c(2010, 2019), season = "SPRING", 
    len.range = c(2:96), age.range = c(0:18)
)

#nova <- odbcConnect(dsn="nova", uid="amiller", pwd="$$Vwda44862003", believeNRows=FALSE)
setwd("/net/home2/dhennen/SAGA/")
species <- read.csv('files/speciesTable.csv')
spp.list <- split(species$SVSPP,species$COMNAME)
strata.list <- read.csv('files/StrataList.csv')
strata.list$YourStrata <- as.character(strata.list$YourStrata)
saved.strata <- strata.list$AllStrata[1:5]
Strata <- readOGR("files","groundfish_strata") ## read sampling strata

get.survey.data.fn <- function(oc = sole, purpose.code = 10){
  q.surveys <- paste0("select cruise6,purpose_code,season,year from mstr_cruise where purpose_code=", purpose.code, 
                      " and cruise6 is not null order by year,cruise6,season")
  survey.view <- sqlQuery(oc,q.surveys)
  #remove duplicates
  survey.view=survey.view[!duplicated(survey.view[,1:4]),]
  return(survey.view)
}
survey.cruises <- get.survey.data.fn()
fall.cruises <- unique(survey.cruises$CRUISE6[survey.cruises$SEASON == 'FALL'])
spring.cruises <- unique(survey.cruises$CRUISE6[survey.cruises$SEASON == 'SPRING'])


S<-input$S<-1
H<-input$H<-3
G<-input$G<-6
len.range <- (input$len.range)
age.range <- (input$age.range)
cruise6 <- survey.cruises$CRUISE6[survey.cruises$SEASON == input$season & 
                                    survey.cruises$YEAR %in% seq(min(input$years),max(input$years))]
spp <- species$SVSPP[species$COMNAME == input$species] #species name as well
#strata.in = paste(input$strata, collapse = "','")
strata.in <- input$strata
spp=spp;oc = sole;strata = strata.in;do.length = TRUE; 
do.age = T; gcf = 1;dcf = 1;vcf = 1;tow_swept_area = 0.01;do.Albatross = F; do.Bigelow=T;
survey = cruise6[1];boot=F;do.BigLen=F;do.AlbLen=F;big.len.calib=NULL;
gcf.n = 1; dcf.n = 1;vcf.n = 1;gcf.w = 1;dcf.w = 1;vcf.w = 1;swept_area = TRUE;Type=1;
 Operation=3;Gear=2


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#choose your year 
i=2
source("helper.R") 
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

#Run this after running get.survey.stratum.estimates.2.fn
        CatchData<-c();Ind.out<-c();IAL.out<-c();VIAL.out<-c();IAA.out<-c()
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


#Compare to STOCKEFF output
library(tidyverse)        
directory

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
set.start.Yr=1982
set.term.Yr<-2019  #Set this to a manual term year if you want one...
nages=7 #ages 1 - 7+
#species information
disp="ADIOS_SV_"
species<-164744 #Species_itis for haddock
stock<-"GOM"
sex="NONE"
#This is the ADIOS! directory prefix - if it changes just make the change here..
dir.ADIOS="file:///net/home0/pdy/pub/STOCKEFF/ADIOS/ADIOS_SV/website/webfiles/"
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

stockeff_naa <- 
  read.csv(paste0(dir.ADIOS,paste0("ADIOS_SV_",species,"_",stock,"_",sex,"_strat_mean_age_auto.csv"))) %>%
  select(YEAR, SEASON, AGE, NO_AT_AGE) %>%
  filter(AGE < 9) %>%
  mutate(AGE = as.character(AGE)) %>%
  # bind_rows({read.csv(paste0(dir.ADIOS,paste0("ADIOS_SV_",species,"_",stock,"_",sex,"_strat_mean_age_auto.csv"))) %>%
  #       select(YEAR, SEASON, AGE, NO_AT_AGE) %>% 
  #       group_by(YEAR, SEASON) %>% 
  #       filter(AGE >= 9) %>% 
  #       summarise(NO_AT_AGE = sum(NO_AT_AGE)) %>%
  #       mutate(AGE = "9+")}) %>%
  mutate(source = "StockEff")

stockeff_naa %>% 
  filter(YEAR==Yeari & SEASON==ifelse(cruise6[i]%in%spring.cruises,"SPRING","FALL"))


