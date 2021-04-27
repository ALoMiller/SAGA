#Testing function for SAGAr. User provides species specfics and this function pulls values from STOCKEFF
#and SAGAr for comparison. Only the arguments in the function call should be required to run this. 
#Although the user may wish to change some of the settings below depending on which comparisons are
#desired. The output will go into a stock specific directory inside a new directory called 'TestHelper'. 
#This function will produce plots (if plot1=T) regardless of differences, but will produce  output csv 
#files only when differences between STOCKEFF and SAGAr exceed tol for each of the indices.
#Note you can grind through all of the STOCKEFF STOCKS by running this:
# #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# require(ROBDC)
# Sys.setenv(ORACLE_HOME="/ora1/app/oracle/product/11.2.0/dbhome_1")
# un="dhennen"
# pw="$$dhe2###QWE"
# sole <- RODBC::odbcConnect(dsn="sole", uid=un, pwd=pw, believeNRows=FALSE)
# #Get species specific specs from STOCKEFF
# q.list <- paste0("select species_itis, stock_abbrev, season, sv_start_year, sv_end_year, sex_type "
#                  ,"from STOCKEFF.I_SV_STOCK_SURVEY_S; ")
# all.spp <- RODBC::sqlQuery(sole,q.list)
# dim(all.spp)
# all.spp = all.spp[all.spp$SEASON%in%c("FALL","SPRING"),] #SAGAr does not do winter survey so far
# sppTab <- read.csv('files/speciesTable.csv')
# all.spp = all.spp[all.spp$SPECIES_ITIS%in%sppTab$SPECIES_ITIS,] #make sure SAGAr has this stock
# 
# 
# 
# for(i in 1:nrow(all.spp)){
#   year = c(ifelse(all.spp$SV_START_YEAR[i]<1968,1968,all.spp$SV_START_YEAR[i]):ifelse(is.na(all.spp$SV_END_YEAR[i])
#                                         ,(as.integer(substr(date(),nchar(date())-4,nchar(date())))-1)
#                                           , all.spp$SV_END_YEAR[i]))
#   speciesItis = all.spp$SPECIES_ITIS[i]
#   stock = all.spp$STOCK_ABBREV[i]
#   season = all.spp$SEASON[i]
#   sex=all.spp$SEX_TYPE[i]
#   t1=try(TestSAGAr(year,speciesItis,stock,season,sex,un,pw))
#   if(attr(t1,"class")=="try-error") print(paste("SAGAr does not run for "
#             ,i,sppTab$COMNAME[which(sppTab$SPECIES_ITIS==all.spp$SPECIES_ITIS[i])]
#             ,speciesItis,stock,season,un,pw))
# }  
# #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

TestSAGAr=function(
  year=NULL #Provide year or vector of years over which to make comparison
  ,speciesItis=NULL #6 digit Speciesitis code for species of interest
  ,stock=NULL #stock of interest: "UNIT","GOM","GBK",etc
  ,season=NULL #"SPRING" or "FALL" are allowed
  ,sex="NONE"
  ,un=NULL #user name for NEFSC Oracle databases eg "amiller"
  ,pw=NULL #pass word for NEFSC Oracle databases eg "xxx123"
  ,tol=1.0 #% absolute difference that causes the function to throw a flag (a "real" difference in indices between Stockeff and SAGAr)
  ,plot1=T #Turn to false to turn off plotting
  ,removeOldFiles = T #Turn to false to keep old versions (some may still be overwritten)
){
#**************************************************************************************************************
  require(RODBC)
  require(tidyverse)
  require(ggforce)        
  #Make sure you are working in SAGAr directory
  source("helper.R") 
  
  #$$$$$$$$$$$$$$$$$$ TESTING LINES $$$$$$$$$$$$$$$$$$$$$$$
  
  #To run line by line make Test=T
  Test=F
  if(Test){
    #speciesItis<-164744 #Species_itis for haddock
    speciesItis<-172933 #Species_itis for halibut
    stock<-"UNIT"    #"GOM"
    sex="NONE"
    season="SPRING"
    year=c(2009:2018)
    tol=1.0
    plot1=T
    removeOldFiles = T
  }
  
  #$$$$$$$$$$$$$$ Connections and Directory Paths $$$$$$$$$$$$$$$$$$$$$$$$$$
  #Sys.setenv(ORACLE_HOME="/ora1/app/oracle/product/11.2.0/dbhome_1")
  sole <- RODBC::odbcConnect(dsn="sole", uid=un, pwd=pw, believeNRows=FALSE)
  
  #This is the STOCKEFF directory prefix - if it changes just make the change here..
  dir.ADIOS="file:///net/home0/pdy/pub/STOCKEFF/ADIOS/ADIOS_SV/website/webfiles/"
  #Get common names to help with naming files
  sppTab <- read.csv('files/speciesTableTestHelper.csv')
  #sppTab <- read.csv('files/speciesTable.csv')
  
  #Create directories to store results 
  if(!file.exists("TestHelper")) system(paste0(" mkdir -p TestHelper;"))
  fn=paste(gsub(" ","_",sppTab$COMNAME[which(sppTab$SPECIES_ITIS==speciesItis)]),speciesItis,stock,sex,season,paste(range(year),collapse="_"),sep="_")
  dir.graphics=paste0("TestHelper/",fn,"/")
  if(!file.exists(dir.graphics)) system(paste0(" mkdir -p ",dir.graphics,";"))
  #Remove files from the directory 
  if(removeOldFiles) system(paste0(" rm ",dir.graphics,"* ;"))
  
  #These are default values; many will be altered below
  disp="ADIOS_SV_"
  do.length = TRUE 
  do.age = T
  gcf = 1
  dcf = 1
  vcf = 1
  tow_swept_area = 0.01
  do.Albatross = F
  do.Bigelow=T #Default to converting Albatross to Bigelow values 
  boot=F
  do.BigLen=F
  do.AlbLen=F
  gcf.n = 1
  dcf.n = 1
  vcf.n = 1
  gcf.w = 1
  dcf.w = 1
  vcf.w = 1
  swept_area = F
  Type=1
  Operation=3
  Gear=2
  S=1
  H=3
  G=6
  Expansion=T
  
  #%%%%%%%%%%%%%%%%% Calibration factors %%%%%%%%%%%%%%%%%%%%%%%#
  #Uses STOCKEFF: I_SV_BGL_LEN_CALIBR_C and I_SV_STOCK_CALIBRATION_S and I_SV_CONST_CALIBR_C
  #Take a look in the Oracle tables to get STOCKEFF calibration values. This is a two step process
  #that depends on weather the calibration is length based or constant over length
  q.calib <- paste0("select species_itis, stock_abbrev, season, sa_year, index_type, calibration_type, calibration_table "
                 ,", sex_type "    
                 ,"from STOCKEFF.I_SV_STOCK_CALIBRATION_S where species_itis = '",speciesItis,"' and stock_abbrev = '",stock, "'"
                 ,"and season = '",season, "';")
  
  cal.view <- RODBC::sqlQuery(sole,q.calib)  
  
  #Now determine what kind of calibration we are dealing with - assume that the most recent SA_YEAR is best value for use.
  cal.view=cal.view[which(cal.view$SA_YEAR==max(cal.view$SA_YEAR)),]
  BGtab=cal.view$CALIBRATION_TABLE[which(cal.view$CALIBRATION_TYPE=="BIGELOW")]
  if(length(BGtab)>1) BGtab=unique(BGtab)
  if(any(BGtab=="I_SV_BGL_LEN_CALIBR_C")) { do.AlbLen=T }     #Length based bigelow calibration (not sure why it is called AlbLen)
  #We have constant Bigelow calibration factors $$ THIS TABLE DOES NOT HAVE STOCK_ABBREV  - IS THAT RIGHT?
 
  
  if(any(BGtab=="I_SV_CONST_CALIBR_C")){
    #Next grab any gear/door/vessel conversion factors
    q.DVGcalib <- paste0("select species_itis, season, index_type, calibration_type "
                      ,", sex_type, START_DATE, END_DATE, CALIBRATION_FACTOR, CALIBRATION_FIELD, CALIBRATION_SIGN " 
                      , ", CALIBRATION_CONVERT , CALIBRATION_OPERATION "    
                      ," from STOCKEFF.I_SV_CONST_CALIBR_C where species_itis = '",speciesItis,"'"
                      ," and season = '",season, "';")
    
    calBigC <- RODBC::sqlQuery(sole,q.DVGcalib) 

    #Do we do Bigelow to Albatross conversion (helper finds the values)    
    if(any(calBigC$CALIBRATION_TYPE=="BIGELOW")) {do.Bigelow=F; do.Albatross=T}
    
    if(any(calBigC$CALIBRATION_TYPE=="DOOR")) {
      dcf.n=calBigC$CALIBRATION_FACTOR[which(calBigC$INDEX_TYPE=="NUMBER" & calBigC$CALIBRATION_TYPE=="DOOR")]
      dcf.w=calBigC$CALIBRATION_FACTOR[which(calBigC$INDEX_TYPE=="WEIGHT" & calBigC$CALIBRATION_TYPE=="DOOR")]
    }
    if(any(calBigC$CALIBRATION_TYPE=="GEAR")) {
      gcf.n=calBigC$CALIBRATION_FACTOR[which(calBigC$INDEX_TYPE=="NUMBER" & calBigC$CALIBRATION_TYPE=="GEAR")]
      gcf.w=calBigC$CALIBRATION_FACTOR[which(calBigC$INDEX_TYPE=="WEIGHT" & calBigC$CALIBRATION_TYPE=="GEAR")]
    }
    if(any(calBigC$CALIBRATION_TYPE=="VESSEL")) {
      vcf.n=calBigC$CALIBRATION_FACTOR[which(calBigC$INDEX_TYPE=="NUMBER" & calBigC$CALIBRATION_TYPE=="VESSEL")]
      vcf.w=calBigC$CALIBRATION_FACTOR[which(calBigC$INDEX_TYPE=="WEIGHT" & calBigC$CALIBRATION_TYPE=="VESSEL")]
    }
  }
  
  #%%%%%%%%%%%%%%%%% Tow evaluation factors factors %%%%%%%%%%%%%%%%%%%%%%%#
  # Uses STOCKEFF: I_SV_STOCK_SHG_TOGA_S
  q.shg <- paste0("select species_itis, stock_abbrev, season, sa_year, year, tow_eval_criteria, tow_eval_value "
                 ,", sex_type "    
                 ,"from STOCKEFF.I_SV_STOCK_SHG_TOGA_S where species_itis = '",speciesItis,"' and stock_abbrev = '",stock, "'"
                 ,"and season = '",season, "';")
  
  shg.view <- RODBC::sqlQuery(sole,q.shg)   
  #Assume the most recent sa_year is the best one to use
  shg.view=shg.view[which(shg.view$SA_YEAR==max(shg.view$SA_YEAR)),]
  #check for oddball values (don't really have a correction for year dependent tow criteria - so this will jsut throw an error)
  if(any(shg.view$TOW_EVAL_VALUE[shg.view$TOW_EVAL_CRITERIA=="SHG"]!="136")) {
    print(paste0("ERROR in SHG criteria for ",fn," Non constant over time!!"))
    #This wil signify a departure from default values so we will just assume they are constant over the AB years
    S=as.integer(substr(shg.view$TOW_EVAL_VALUE[which(shg.view$TOW_EVAL_VALUE[shg.view$TOW_EVAL_CRITERIA=="SHG"]!="136")],1,1))
    H=as.integer(substr(shg.view$TOW_EVAL_VALUE[which(shg.view$TOW_EVAL_VALUE[shg.view$TOW_EVAL_CRITERIA=="SHG"]!="136")],2,2))
    G=as.integer(substr(shg.view$TOW_EVAL_VALUE[which(shg.view$TOW_EVAL_VALUE[shg.view$TOW_EVAL_CRITERIA=="SHG"]!="136")],3,3))
  }
  if(any(shg.view$TOW_EVAL_VALUE[shg.view$TOW_EVAL_CRITERIA=="TOGA"]!="132X")) {
    print(paste0("ERROR in TOGA criteria for ",fn," Non constant over time!!"))
    #This wil signify a departure from default values so we will just assume they are constant over the AB years
    TYPE=as.integer(substr(shg.view$TOW_EVAL_VALUE[which(shg.view$TOW_EVAL_VALUE[shg.view$TOW_EVAL_CRITERIA=="TOGA"]!="132X")],1,1))
    OPERATION=as.integer(substr(shg.view$TOW_EVAL_VALUE[which(shg.view$TOW_EVAL_VALUE[shg.view$TOW_EVAL_CRITERIA=="TOGA"]!="132X")],2,2))
    GEAR=as.integer(substr(shg.view$TOW_EVAL_VALUE[which(shg.view$TOW_EVAL_VALUE[shg.view$TOW_EVAL_CRITERIA=="TOGA"]!="132X")],3,3))
  }
  #The default values should work for most stocks
  

  #Get species specific specs from STOCKEFF
  YrPaste=paste0("'",year,"'",collapse = ",")
  q.spec <- paste0("select common_name, species_itis, stock_abbrev, season, year, stratum, tow_eval_criteria, tow_eval_value "
                 ,"from STOCKEFF.I_SV_PROJECTED_SPECS_S where species_itis = '",speciesItis,"' and stock_abbrev = '",stock, "'"
                 ," and year in (",YrPaste,");")
  
  spec.view <- RODBC::sqlQuery(sole,q.spec) 
  #print(summary(spec.view))
  #Pulling min and max values from SAGAr data tables:
  ageRange=(sppTab[which(sppTab$SPECIES_ITIS==speciesItis),c("MINA","MAXA")])
  LenRange=(sppTab[which(sppTab$SPECIES_ITIS==speciesItis),c("MINL","MAXL")])
  if(any(is.na(ageRange))) do.age=F
  if(any(is.na(LenRange))) do.length=F
  
  get.survey.data.fn <- function(oc = sole, purpose.code = 10){
    q.surveys <- paste0("select cruise6,purpose_code,season,year from mstr_cruise where purpose_code=", purpose.code, 
                        " and cruise6 is not null order by year,cruise6,season")
    survey.view <- RODBC::sqlQuery(oc,q.surveys)
    #remove duplicates
    survey.view=survey.view[!duplicated(survey.view[,1:4]),]
    return(survey.view)
  }
  survey.cruises <- get.survey.data.fn()
  #print(summary(survey.cruises))
  fall.cruises <- unique(survey.cruises$CRUISE6[survey.cruises$SEASON == 'FALL'])
  spring.cruises <- unique(survey.cruises$CRUISE6[survey.cruises$SEASON == 'SPRING'])
  
  big.len.calib <- read.csv('files/BigelowLenCalib.csv')
  #This may not work for all species. 
  if(!speciesItis%in%big.len.calib$SPECIES_ITIS) do.AlbLen=F
  
  #Check STOCKEFF for age and length plus groups... $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  if(do.length) len.range <- c(LenRange$MINL:LenRange$MAXL)
  if(do.age) age.range <- c(ageRange$MINA:ageRange$MAXA)
  cruise6 <- survey.cruises$CRUISE6[survey.cruises$SEASON == paste(season) & 
                    survey.cruises$YEAR %in% seq(min(sort(unique(spec.view$YEAR))),max(sort(unique(spec.view$YEAR))))]
  spp <- (sppTab[which(sppTab$SPECIES_ITIS==speciesItis),c("SVSPP")]) #species svspp code as well
  strata.in <- paste0("0",unique(spec.view$STRATUM))
  strata = strata.in
  oc = sole;
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  CatchData<-c();Ind.out<-c();IAL.out<-c();VIAL.out<-c();IAA.out<-c();Expand=c()
  
  for(c6 in cruise6){
  
    x.out<- get.survey.stratum.estimates.2.fn(spp=spp,
                                                      survey = c6, 
                                                      oc = sole, 
                                                      strata = strata.in,
                                                      lengths = len.range,
                                                      ages = age.range,
                                                      do.length = do.length, 
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
                                                      species=sppTab, 
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
    #if(nrow(x.out$catchdata[[1]])>0) CatchData<-(rbind(CatchData,x.out$catchdata[[1]])) #keep the catch data for later output
    
    #Take the important parts from x.out to generate an index over time.
    Yeari=as.integer(substr(paste(c6),1,4))
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
    Svars=as.data.frame(t(Svars))
    SMns=as.data.frame(t(SMns))
    Ind.out<-rbind(Ind.out,data.frame("Year"=Yeari,"nTows"=Tows
                             ,"EXPCATCHNUM"=SMns[1,1],"EXPCATCHWT"=SMns[1,2]
                             ,"VARNUM"=Svars[1,1],"VARWT"=Svars[1,2])) 
    #The indices at length require similar manipulation
    goodRows=(x.out$out[,"m"]>0)
    Lframe=data.frame(t(colSums(x.out$Nal.hat.stratum/sum(x.out$out[,"M"]))))
    names(Lframe)=paste0("Len",len.range)
    IAL.out<-rbind(IAL.out,data.frame("Year"=Yeari,"nTows"=Tows,Lframe
                             ,"Total"=sum(colSums(x.out$Nal.hat.stratum/sum(x.out$out[,"M"])))))  #Add a "Total" which is the index over the sizes of interest
    
    
    #divide by the stratum area to get unexpanded numbers at length
    VIAL.out<-rbind(VIAL.out,c(Yeari,Tows,colSums(x.out$V.Nal.stratum/sum(x.out$out[,"M"])^2)
                               ,"Total"=sum(colSums(x.out$V.Nal.stratum/sum(x.out$out[,"M"])^2)))) #remove stratum area expansion
    
    IAA.out=as.data.frame(IAA.out)
    if(do.age) { 
      IAA.out<-rbind(IAA.out,c(Yeari,Tows,colSums(x.out$Naa.hat.stratum[,2:ncol(x.out$Naa.hat.stratum)]/sum(x.out$out[,"M"]))
                               ,"Total"=sum(colSums(x.out$Naa.hat.stratum[,2:ncol(x.out$Naa.hat.stratum)]/sum(x.out$out[,"M"])))))
      names(IAA.out)=c('Year','nTows',paste0("Age",age.range),'Total')
      #print(IAA.out)
    } 
  
    #Keep the expansion factor for unsampled strata on hand for each year
    Expand=c(Expand,x.out$expand)
  }
  
  #If the user wishes to expand over unsampled strata
  if(Expansion){
    expnd=Expand
  } else expnd=rep(1.,nrow(Expand))
  
  IAL.out[,2:ncol(IAL.out)]=IAL.out[,2:ncol(IAL.out)]*expnd #Expand the total to cover unsampled strata (if desired)
  #Not really checking variances currently
  #VIAL.out[,ncol(VIAL.out)]=VIAL.out[,ncol(VIAL.out)]*expnd^2 #Expand the total to cover unsampled strata (if desired)

  if(do.age) {
    IAA.out[,2:ncol(IAA.out)]=IAA.out[,2:ncol(IAA.out)]*expnd #Expand the total to cover unsampled strata (if desired)
  } else IAA.out="No ages"
  #Move the comparison to a function so we can use it in the SAGAr shiny later...
  CompareStockEffSagar(
    dir.ADIOS=dir.ADIOS
    ,dir.graphics=dir.graphics
    ,fn=fn
    ,year=year
    ,season=season
    ,do.length=do.length
    ,do.age=do.age
    ,Ind.out=Ind.out
    ,IAL.out=IAL.out
    ,IAA.out=IAA.out
    ,plot1 = plot1
    ,tol=tol
  )


}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CompareStockEffSagar=function(
  dir.ADIOS=NULL
  ,dir.graphics=NULL
  ,fn=NULL
  ,year=NULL
  ,season=NULL
  ,do.length=NULL
  ,do.age=NULL
  ,Ind.out=NULL
  ,IAL.out=NULL
  ,IAA.out=NULL
  ,plot1=F
  ,tol=0.01
){
    #Compare to STOCKEFF output: Plots
  #Start with overall index - numbers 
  stockeff_ind<-
    read.csv(paste0(dir.ADIOS,paste0("ADIOS_SV_",speciesItis,"_",stock,"_",sex,"_strat_mean.csv"))) %>%
    select(YEAR, SEASON, INDEX_TYPE, INDEX, SURVEY) %>%
    mutate(source = "StockEff") %>% 
    filter(YEAR%in%year & SEASON==season & SURVEY%in%c("NMFS fall BTS", "NMFS spring BTS") ) %>%
    select(-SURVEY) %>%
    spread(INDEX_TYPE,INDEX) %>%
    rename(NUM='Abundance (numbers/tow)') %>%
    rename(WT='Biomass (kg/tow)')
  
  
  saga_ind<-
    Ind.out %>%
    select(-nTows, -VARWT, -VARNUM) %>%
    mutate(SEASON=season) %>%
    mutate(source = "SAGA Shiny App") %>%
    rename(NUM = EXPCATCHNUM) %>%
    rename(WT = EXPCATCHWT) %>%  
    rename(YEAR = Year)
    
  both_ind <- 
    rbind(stockeff_ind, saga_ind) %>%
    mutate(YEAR=as.integer(YEAR))
  
  if(plot1) {
    ggplot(both_ind, 
      aes(x = YEAR, y = NUM, color = source)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      ylab("Stratified mean numbers per tow") +
      xlab("Year") +
      ggtitle("Abundance index comparison")
    ggsave(paste0(dir.graphics,fn,"Abun.png"))
  }
  
  # Plot percent differences in indices
  pc_diff_num <-
    both_ind %>%  
    select(-WT) %>%
    spread(source, NUM) %>%
    mutate(`Percent difference` = 100 * (`SAGA Shiny App` - StockEff)/StockEff ) %>%
    #select(YEAR, SEASON,`SAGA vs StockEff`) %>% 
    #gather(comparison, `Percent difference`, -YEAR, -SEASON) %>%
    mutate(`Absolute percent difference` = abs(`Percent difference`))
  
  if(plot1) {
    ggplot(pc_diff_num , 
      aes(x = YEAR, y = `Absolute percent difference`, color = `Percent difference`)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      ggtitle("Percent N per tow differences")
    ggsave(paste0(dir.graphics,fn,"AbunAbsDiff.png"))
  }
  
  if(any(na.omit(pc_diff_num$`Absolute percent difference`)>tol)) {
    write.csv(pc_diff_num[which(pc_diff_num$`Absolute percent difference`>tol),]
              ,paste0(dir.graphics,"AbundanceIndexDifferences.csv")
              ,row.names = F
              )
  }
  
  #Now WT
  if(plot1) {
    ggplot(both_ind, 
      aes(x = YEAR, y = WT, color = source)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      ylab("Stratified mean weight per tow") +
      xlab("Year") +
      ggtitle("Biomass index comparison")
    ggsave(paste0(dir.graphics,fn,"Wt.png"))
  }
  # Plot percent differences in indices
  pc_diff_wt <-
    both_ind %>%  
    select(-NUM) %>%
    spread(source, WT) %>%
    mutate(`Percent difference` = 100 * (`SAGA Shiny App` - StockEff)/StockEff ) %>%
    #select(YEAR, SEASON,`SAGA Shiny App`,StockEff , `SAGA vs StockEff`) %>%
    #gather(comparison, `Percent difference`, -YEAR, -SEASON) %>%
    mutate(`Absolute percent difference` = abs(`Percent difference`))
  
  if(plot1) {
    ggplot(pc_diff_wt , 
      aes(x = YEAR, y = `Absolute percent difference`, color = `Percent difference`)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      ggtitle("Percent kg per tow differences")
    ggsave(paste0(dir.graphics,fn,"WtAbsDiff.png"))
  }
  #Report differences > tol
  if(any(na.omit(pc_diff_num$`Absolute percent difference`)>tol)) {
    write.csv(pc_diff_wt[which(pc_diff_wt$`Absolute percent difference`>tol),]
              ,paste0(dir.graphics,"BiomassIndexDifferences.csv")
              ,row.names = F
              )
  }
  
  #Indices at length
  if(do.length){
    stockeff_nal<-
      read.csv(paste0(dir.ADIOS,paste0("ADIOS_SV_",speciesItis,"_",stock,"_",sex,"_strat_mean_length.csv"))) %>%
      select(YEAR, SEASON, LENGTH, NO_AT_LENGTH, SURVEY) %>%
      mutate(source = "StockEff") %>% 
      filter(YEAR%in%year & SEASON==season & SURVEY%in%c("NMFS fall BTS", "NMFS spring BTS")) %>%
      select(-SURVEY)
    
    saga_nal <-
      IAL.out %>%
      select(-nTows, -Total) %>%
      gather(LENGTH, NO_AT_LENGTH, -Year) %>%
      rename(YEAR = Year) %>%
      mutate(LENGTH = as.numeric(substr(LENGTH,4,99))) %>%
      mutate(source = "SAGA Shiny App") %>%
      mutate(SEASON = season) %>%
      arrange(LENGTH)
    
    both_nal <- 
      rbind(stockeff_nal, saga_nal) %>%
      mutate(LENGTH=as.integer(LENGTH))
    
    if(plot1==T) {
      par(mar=c(4,5,4,2)+0.1,cex.axis=1.5,cex.lab=2,cex.main=2)
      pdf(paste0(dir.graphics,fn,"_compare_NAL.pdf"))
      for(p in 1:ceiling(length(unique(both_nal$YEAR))/12)) {  
        print(ggplot(both_nal, 
          aes(x = LENGTH, y = NO_AT_LENGTH, color = source)) +
          geom_line() +
          geom_point() +
          ggforce::facet_wrap_paginate(~ YEAR, scales = "free_y",nrow=4,ncol=3
                                         ,page=p) +
          theme_bw() +
          ylab("Stratified mean numbers-at-length") +
          xlab("Length") +
          ggtitle("NAL comparison")
        )
      }
      dev.off()
    }
    
    # Plot percent differences in indices
    pc_diff_nal <-
      both_nal %>%
      spread(source, NO_AT_LENGTH) %>%
      mutate(`Percent difference` = 100 * (`SAGA Shiny App` - StockEff)/StockEff ) %>%
      #select(YEAR, SEASON, LENGTH,`SAGA Shiny App`,StockEff , `SAGA vs StockEff`) %>%
      #gather(comparison, `Percent difference`, -YEAR, -SEASON, -LENGTH) %>%
      mutate(`Absolute percent difference` = abs(`Percent difference`))
    
    if(plot1) {
     pdf(paste0(dir.graphics,fn,"_diff_NAL.pdf"))  
      for(p in 1:ceiling(length(unique(pc_diff_nal$YEAR))/12)) {
        print(ggplot(pc_diff_nal , 
               aes(x = LENGTH, y = `Absolute percent difference`, color = `Percent difference`)) +
          ggforce::facet_wrap_paginate(~ YEAR, scales = "free_y",nrow=4,ncol=3
                                       ,page=p) +
          geom_line() +
          geom_point() +
          theme_bw() +
          ggtitle("Percent NAL differences")
          )
      }
      dev.off()
    }
    
    if(any(na.omit(pc_diff_nal$`Absolute percent difference`)>tol)) {
      write.csv(pc_diff_nal[which(pc_diff_nal$`Absolute percent difference`>tol),]
                ,paste0(dir.graphics,"AbundanceAtLengthDifferences.csv")
                ,row.names = F
                )
    }
  }  
  
  #_________________________________________________________________________________________
  #Indices at age
  if(do.age){
      stockeff_naa <- 
      read.csv(paste0(dir.ADIOS,paste0("ADIOS_SV_",speciesItis,"_",stock,"_",sex,"_strat_mean_age_auto.csv"))) %>%
      select(YEAR, SEASON, AGE, NO_AT_AGE, SURVEY) %>%
      #filter(AGE < 9) %>%
      #mutate(AGE = as.character(AGE)) %>%
      mutate(source = "StockEff") %>% 
      filter(YEAR%in%year & SEASON==season & SURVEY%in%c("NMFS fall BTS", "NMFS spring BTS")) %>%
      select(-SURVEY) %>%   
      mutate(AGE = as.integer(AGE))
    
    saga_naa <-
      IAA.out %>%
      select(-nTows, -Total) %>%
      gather(AGE, NO_AT_AGE, -Year) %>%
      rename(YEAR = Year) %>%
      mutate(AGE = as.numeric(substr(AGE,4,99))) %>%
      mutate(AGE = as.character(AGE)) %>%
      mutate(source = "SAGA Shiny App") %>%
      mutate(SEASON = season) %>%
      arrange(AGE)
    
    both_naa <- 
      rbind(stockeff_naa, saga_naa) %>%
      mutate(AGE=as.integer(AGE))
    
    if(plot1) {
      pdf(paste0(dir.graphics,fn,"_compare_NAA.pdf"))
      for(p in 1:ceiling(length(unique(pc_diff_nal$YEAR))/12)) {
        print(ggplot(both_naa, 
               aes(x = AGE, y = NO_AT_AGE, color = source)) +
          geom_line() +
          geom_point() +
          ggforce::facet_wrap_paginate(~ YEAR, scales = "free_y",nrow=4,ncol=3
                                         ,page=p) +
          theme_bw() +
          ylab("Stratified mean numbers-at-age") +
          xlab("Age") +
          ggtitle("NAA comparison")
        )
      }
      dev.off()
    }
    
    # Plot percent differences in indices
    pc_diff_naa <-
      both_naa %>%
      spread(source, NO_AT_AGE) %>%
      mutate(`Percent difference` = 100 * (`SAGA Shiny App` - StockEff)/StockEff ) %>%
      #select(YEAR, SEASON, AGE,`SAGA Shiny App`,StockEff , `SAGA without adjustment vs StockEff`) %>%
      #gather(comparison, `Percent difference`, -YEAR, -SEASON, -AGE) %>%
      mutate(`Absolute percent difference` = abs(`Percent difference`))
    
    if(plot1) {
      pdf(paste0(dir.graphics,fn,"diff_NAA.pdf"))     
      for(p in 1:ceiling(length(unique(pc_diff_nal$YEAR))/12)) {
        print(ggplot(pc_diff_naa , 
               aes(x = AGE, y = `Absolute percent difference`, color = `Percent difference`)) +
          ggforce::facet_wrap_paginate(~ YEAR, scales = "free_y",nrow=4,ncol=3
                                         ,page=p) +
          geom_line() +
          geom_point() +
          theme_bw() +
          ggtitle("Percent NAA differences")
        )
      }
      dev.off()
    }
    if(any(na.omit(pc_diff_naa$`Absolute percent difference`)>tol)) {
      write.csv(pc_diff_naa[which(pc_diff_naa$`Absolute percent difference`>tol),]
                ,paste0(dir.graphics,"AbundanceAtAgeDifferences.csv")
                ,row.names = F
                )
    }
  }
}

